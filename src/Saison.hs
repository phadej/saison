{-# OPTIONS_GHC -Wall -O2 #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Stream Aeson, fruity, spicy, well carbonated.
--
-- Saison represents JSON document as well-formed token stream.
-- This approach is potentially faster than document model (i.e. representing
-- document as an ADT) used in @aeson@. Also the approach is more
-- flexible, as more structure is preseved, especially key-value
-- pairs in records are not ordered.
--
-- Saison is proof-of-concept package at the moment.
--
-- The tokens aren't exactly as in JSON, but in similar grammar.
-- There are no commas, but each item in arrays is prepended
-- with zero-width marker (to differentiate from the end):
--
-- @
-- VALUE   = LITERAL | TEXT | NUMBER | "[" ARRAY | "{" RECORD
-- LITERAL = "null"
--         | "true"
--         | "false"
-- TEXT    = ...
-- NUMBER  = ...
-- ARRAY   = ITEM VALUE ARRAY
--         | "]"
-- ITEM    = epsilon
-- RECORD  = KEY VALUE RECORD
--         | "}"
-- KEY     = TEXT
-- @
--
-- Haskell types reflect this grammar:
--
-- * @VALUE@ is 'Tokens'
-- * @LITERAL@ is 'Lit'
-- * @ARRAY@ is 'TkArray'
-- * @RECORD@ is 'TkRecord'.
--
module Saison (
    -- * Types
    Tokens (..),
    Lit (..),
    TkArray (..),
    TkRecord (..),
    -- * Conversion to/from Value
    toValue,
    fromValue,
    -- * Parsing
    toEitherValue,
    tokens,
    )where

import Data.Aeson.Parser.Internal (jstring_, scientific)
import Data.ByteString            (ByteString)
import Data.Scientific            (Scientific)
import Data.Text                  (Text)
import Data.Void                  (Void, absurd)
import Data.Word                  (Word8)

import qualified Data.Aeson                 as A
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Unsafe     as BS.Unsafe
import qualified Data.HashMap.Strict        as HM
import qualified Data.Vector                as V

-------------------------------------------------------------------------------
-- Tokens
-------------------------------------------------------------------------------

-- | Tokens.
--
-- Note: 'Lit' exists to make 'Tokens' have only 6 constructors.
-- This may or may not have impact on performance.
data Tokens k e
    = TkLit !Lit k
    | TkText !Text k
    | TkNumber !Scientific k
    | TkArrayOpen (TkArray k e)
    | TkRecordOpen (TkRecord k e)
    | TkErr e
  deriving (Eq, Show)

-- | Literals. @null@, @true@, @false@.
data Lit = LitNull | LitTrue | LitFalse
  deriving (Eq, Show)

-- | Array tokens.
data TkArray k e
    = TkItem (Tokens (TkArray k e) e)
    | TkArrayEnd k
    | TkArrayErr e
  deriving (Eq, Show)

-- | Record tokens.
data TkRecord k e
    = TkPair !Text (Tokens (TkRecord k e) e)
    | TkRecordEnd k
    | TkRecordErr e
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- to Value
-------------------------------------------------------------------------------

-- | Convert 'Tokens' to @aeson's@ 'A.Value'.
--
-- This is an evidence that 'Tokens' encoding is sound.
toValue :: Tokens b Void -> A.Value
toValue t0 = go t0 (\v _ -> v) where
    go :: Tokens k Void -> (A.Value -> k -> r) -> r
    go (TkLit LitNull k)  f = f A.Null k
    go (TkLit LitTrue k)  f = f (A.Bool True) k
    go (TkLit LitFalse k) f = f (A.Bool False) k
    go (TkText t k)       f = f (A.String t) k
    go (TkNumber n k)     f = f (A.Number n) k
    go (TkArrayOpen arr)  f = goA 0 id arr $ \n xs k -> f (A.Array (V.fromListN n xs)) k
    go (TkRecordOpen rec) f = goR id rec $ \xs k -> f (A.Object (HM.fromList xs)) k
    go (TkErr e)          _ = absurd e

    goA :: Int                           -- size accumulator
        -> ([A.Value] -> [A.Value])      -- dlist accumulator
        -> TkArray k Void                -- array tokens
        -> (Int -> [A.Value] -> k -> r)  -- continuation
        -> r
    goA !n !acc (TkItem toks)  f = go toks $ \v k -> goA (succ n) (acc . (v :)) k f
    goA !n !acc (TkArrayEnd k) f = f n (acc []) k
    goA !_ !_   (TkArrayErr e) _ = absurd e

    goR :: ([(Text, A.Value)] -> [(Text, A.Value)])
        -> TkRecord k Void
        -> ([(Text, A.Value)] -> k -> r)
        -> r
    goR !acc (TkPair t toks) f = go toks $ \v k -> goR (acc . ((t, v) :)) k f
    goR !acc (TkRecordEnd k) f = f (acc []) k
    goR !_   (TkRecordErr e) _ = absurd e

-- | Opposite direction of 'toValue'.
fromValue :: A.Value -> Tokens () a
fromValue = go () where
    go :: k -> A.Value -> Tokens k a
    go k A.Null         = TkLit LitNull k
    go k (A.Bool True)  = TkLit LitTrue k
    go k (A.Bool False) = TkLit LitFalse k
    go k (A.String t)   = TkText t k
    go k (A.Number n)   = TkNumber n k
    go k (A.Array xs)   = TkArrayOpen (V.foldr (\v ys -> TkItem (go ys v)) (TkArrayEnd k) xs)
    go k (A.Object xs)  = TkRecordOpen (HM.foldrWithKey (\i v ys -> TkPair i (go ys v)) (TkRecordEnd k) xs)

-- | Convert to 'Value', from 'Tokens' potentially containing an error.
-- Also option to check the left over.
toEitherValue :: (k -> Maybe e) -> Tokens k e -> Either e A.Value
toEitherValue check t0 = go t0 Left $ \v end -> case check end of
    Nothing -> Right v
    Just e  -> Left e
  where
    go :: Tokens k e -> (e -> r) -> (A.Value -> k -> r) -> r
    go (TkLit LitNull k)  _ f = f A.Null k
    go (TkLit LitTrue k)  _ f = f (A.Bool True) k
    go (TkLit LitFalse k) _ f = f (A.Bool False) k
    go (TkText t k)       _ f = f (A.String t) k
    go (TkNumber n k)     _ f = f (A.Number n) k
    go (TkArrayOpen arr)  g f = goA 0 id arr g $ \n xs k -> f (A.Array (V.fromListN n xs)) k
    go (TkRecordOpen rec) g f = goR id rec g $ \xs k -> f (A.Object (HM.fromList xs)) k
    go (TkErr e)          g _ = g e

    goA :: Int                           -- size accumulator
        -> ([A.Value] -> [A.Value])      -- dlist accumulator
        -> TkArray k e                   -- array tokens
        -> (e -> r)                      -- error continuation
        -> (Int -> [A.Value] -> k -> r)  -- success continuation
        -> r
    goA !n !acc (TkItem toks)  g f = go toks g $ \v k -> goA (succ n) (acc . (v :)) k g f
    goA !n !acc (TkArrayEnd k) _ f = f n (acc []) k
    goA !_ !_   (TkArrayErr e) g _ = g e

    goR :: ([(Text, A.Value)] -> [(Text, A.Value)])
        -> TkRecord k e
        -> (e -> r)
        -> ([(Text, A.Value)] -> k -> r)
        -> r
    goR !acc (TkPair t toks) g f = go toks g $ \v k -> goR (acc . ((t, v) :)) k g f
    goR !acc (TkRecordEnd k) _ f = f (acc []) k
    goR !_   (TkRecordErr e) g _ = g e

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

-- | Lex (and parse) the 'ByteString' into 'Tokens' stream.
--
-- The rest of input is in @'Maybe' 'ByteString'@. Errors are simple textual errors atm.
--
-- * TODO: introduce abstraction for @Parser tok k = BS -> (BS -> k) -> tok k String@
-- * TODO: try with stack of parsers, and not making new continuations. Will it be faster?
tokens :: ByteString -> Tokens (Maybe ByteString) String
tokens bs0 = goT bs0 $ \bs' ->
    if BS.null bs'
    then Nothing
    else Just bs'
  where
    goT :: ByteString
        -> (ByteString -> k)
        -> Tokens k String
    goT bs k = case uncons bs of
        Nothing -> TkErr "Unexpected end-of-input, expecting JSON value"
        Just (!w, !bs') -> tokenCase w bs' k

    tokenCase 123 {- { -} bs k = TkRecordOpen (goR bs k)
    tokenCase 91  {- [ -} bs k = TkArrayOpen (goA bs k)
    tokenCase 34  {- " -} bs k = case Atto.parse jstring_ bs of
        Atto.Done bs1 t   -> TkText t (k bs1)
        Atto.Fail _ _ err -> TkErr $ "Error parsing text literal: " ++ err
        Atto.Partial {}   -> TkErr "Unexpected end-of-input while parsing text literal"
    tokenCase w           bs k
        | 48 <= w && w <= 75 || w == 45 = case Atto.parse scientific bs of
            Atto.Done bs1   s -> TkNumber s (k bs1)
            Atto.Fail _ _ err -> TkErr $ "Error parsing number literal: " ++ err
            Atto.Partial {}   -> TkErr "Unexpected end-of-input while parsing number literal"
    tokenCase 110 {- n -} bs k
        | Just bs1 <- stripPrefix "ull" 3 bs = TkLit LitNull (k bs1)
    tokenCase 116 {- t -} bs k
        | Just bs1 <- stripPrefix "rue" 3 bs = TkLit LitTrue (k bs1)
    tokenCase 102 {- f -} bs k
        | Just bs1 <- stripPrefix "alse" 4 bs = TkLit LitFalse (k bs1)

    tokenCase w bs _ = TkErr $ "Unexpected " ++ BS8.unpack (BS.cons w (BS.take 29 bs)) ++ ", expecting JSON value"

    -- Array
    goA :: ByteString
        -> (ByteString -> k)
        -> TkArray k String
    goA bs k = case uncons bs of
        Nothing -> TkArrayErr "Unexpected end-of-input, expecting JSON value or ]"
        Just (93, !bs1) -> TkArrayEnd (k bs1)
        Just (w,  !bs1) -> TkItem $ tokenCase w bs1 $ \bs2 -> goA1 bs2 k

    goA1 :: ByteString
         -> (ByteString -> k)
         -> TkArray k String
    goA1 bs k = case uncons bs of
        Nothing -> TkArrayErr "Unexpected end-of-input, expecting , or ]"
        Just (93, !bs1) -> TkArrayEnd (k bs1)
        Just (44, !bs1) -> TkItem $ goT bs1 $ \bs2 -> goA1 bs2 k
        _               -> TkArrayErr $ "Unexpected " ++ show (BS8.unpack (BS.take 30 bs)) ++ ", expecting , or ]"

    -- Record

    goR :: ByteString
        -> (ByteString -> k)
        -> TkRecord k String
    goR bs k = case uncons bs of
        Nothing -> TkRecordErr "Unexpected end-of-input, expecting record key or }"
        Just (34,  !bs1) -> goRK bs1 k           -- "
        Just (125, !bs1) -> TkRecordEnd (k bs1)  -- }
        Just _           -> TkRecordErr $ "Unexpected " ++ show (BS8.unpack (BS.take 30 bs)) ++ ", expecting key literal or }"

    goR1 :: ByteString
         -> (ByteString -> k)
         -> TkRecord k String
    goR1 bs k = case uncons bs of
        Nothing -> TkRecordErr "Unexpected end-of-input, expecting , or }"
        Just (44, !bs1)  -> case uncons bs1 of
            Nothing         -> TkRecordErr "Unexpected end-of-input, expecting key literal"
            Just (34, !bs2) -> goRK bs2 k
            Just _          -> TkRecordErr $ "Unexpected " ++ show (BS8.unpack (BS.take 30 bs)) ++ ", expecting key literal"
        Just (125, !bs1) -> TkRecordEnd (k bs1)
        _                -> TkRecordErr $ "Unexpected " ++ show (BS8.unpack (BS.take 30 bs)) ++ ", expecting , or }"

    goRK :: ByteString
         -> (ByteString -> k)
         -> TkRecord k String
    goRK bs1 k = case Atto.parse jstring_ bs1 of
        Atto.Done bs2 t   -> case uncons bs2 of
            Nothing         -> TkRecordErr "Unexpected end-of-input, expecting :"
            Just (58, !bs3) -> TkPair t $ goT bs3 $ \bs4 -> goR1 bs4 k
            Just _          -> TkRecordErr $ "Unexpected " ++ show (BS8.unpack (BS.take 30 bs2)) ++ ", expecting :"
        Atto.Fail _ _ err -> TkRecordErr $ "Error parsing key literal:" ++ err
        Atto.Partial {}   -> TkRecordErr "Unexpected end-of-input while parsing key literal"

    stripPrefix :: ByteString -> Int -> ByteString -> Maybe ByteString
    stripPrefix pfx n bs | BS.isPrefixOf pfx bs = Just (BS.Unsafe.unsafeDrop n bs)
                         | otherwise            = Nothing
    {-# INLINE stripPrefix #-}

    uncons :: ByteString -> Maybe (Word8, ByteString)
    uncons = BS.uncons . skipSpace
    {-# INLINE uncons #-}

-- | Strip leading (ASCII) space
skipSpace :: BS.ByteString -> BS.ByteString
skipSpace bs = case BS.uncons bs of
    Just (w, bs') | w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
        -> skipSpace bs'
    _ -> bs
{-# INLINE skipSpace #-}
