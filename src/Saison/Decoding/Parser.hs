{-# OPTIONS_GHC -Wall -O2 #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Parser from strict 'ByteString' to 'Tokens'.
module Saison.Decoding.Parser where

import Data.Aeson.Parser.Internal (jstring_, scientific)
import Data.ByteString            (ByteString)
import Data.Word                  (Word8)

import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Unsafe     as BS.Unsafe

import Saison.Decoding.Tokens

-- | Lex (and parse) the 'ByteString' into 'Tokens' stream.
--
-- The rest of input is in @'Maybe' 'ByteString'@. Errors are simple textual errors atm.
--
-- * TODO: try with stack of parsers, and not making new continuations. Will it be faster?
-- * TODO: make an 'Error' data type.
--
tokens :: ByteString -> Tokens ByteString String
tokens bs0 = goT bs0 id where
    goT :: Parser Tokens k
    goT bs' k = case uncons bs of
        Nothing -> tkErr "Unexpected end-of-input, expecting JSON value"
        Just (!w, !bs1) -> tokenCase w bs1 bs k
      where
        bs = skipSpace bs'

    tokenCase
        :: Word8              -- head
        -> ByteString         -- tail
        -> ByteString         -- whole input, needed for number parsing
        -> (ByteString -> k)  -- continuation
        -> Tokens k String
    tokenCase 123 {- { -} !bs !_   k = TkRecordOpen (goR bs k)
    tokenCase 91  {- [ -} bs _   k = TkArrayOpen (goA bs k)
    tokenCase 34  {- " -} bs _   k = case Atto.parse jstring_ bs of
        Atto.Done bs1 t   -> TkText t (k bs1)
        Atto.Fail _ _ err -> tkErr $ "Error parsing text literal: " ++ err
        Atto.Partial {}   -> tkErr "Unexpected end-of-input while parsing text literal"
    tokenCase w           _  wbs k
        | 48 <= w && w <= 75 || w == 45 = case attoParse scientific wbs of
            Atto.Done bs1   s -> TkNumber s (k bs1)
            Atto.Fail _ _ err -> tkErr $ "Error parsing number literal " ++ showBeginning wbs ++ ": " ++ err
            Atto.Partial {}   -> tkErr "Unexpected end-of-input while parsing number literal"
    tokenCase 110 {- n -} bs _   k
        | Just bs1 <- stripPrefix "ull" 3 bs = TkLit LitNull (k bs1)
    tokenCase 116 {- t -} bs _   k
        | Just bs1 <- stripPrefix "rue" 3 bs = TkLit LitTrue (k bs1)
    tokenCase 102 {- f -} bs _   k
        | Just bs1 <- stripPrefix "alse" 4 bs = TkLit LitFalse (k bs1)

    tokenCase w           bs _   _ = tkErr $ "Unexpected " ++ show (BS.cons w (BS.take 29 bs)) ++ ", expecting JSON value"

    -- Array
    goA :: Parser TkArray k
    goA bs' k = case BS.uncons bs of
        Nothing         -> tkErrEOF "JSON value or ]"
        Just (93, !bs1) -> TkArrayEnd (k bs1)
        Just (w,  !bs1) -> TkItem $ tokenCase w bs1 bs $ \bs2 -> goA1 bs2 k
      where
        bs = skipSpace bs'

    goA1 :: Parser TkArray k
    goA1 bs' k = case BS.uncons bs of
        Nothing         -> tkErrEOF ", or ]"
        Just (93, !bs1) -> TkArrayEnd (k bs1)
        Just (44, !bs1) -> TkItem $ goT bs1 $ \bs2 -> goA1 bs2 k
        _               -> tkErrBS bs ", or ]"
      where
        bs = skipSpace bs'

    -- Record

    goR :: Parser TkRecord k
    goR bs k = case uncons bs of
        Nothing          -> tkErrEOF "record key literal or }"
        Just (34,  !bs1) -> goRK bs1 k           -- "
        Just (125, !bs1) -> TkRecordEnd (k bs1)  -- }
        Just _           -> tkErrBS bs "record key literal or }"

    goR1 :: Parser TkRecord k
    goR1 bs k = case uncons bs of
        Nothing -> tkErr "Unexpected end-of-input, expecting , or }"
        Just (44, !bs1)  -> case uncons bs1 of
            Nothing         -> tkErrEOF "key literal"
            Just (34, !bs2) -> goRK bs2 k
            Just _          -> tkErrBS bs "key literal"
        Just (125, !bs1) -> TkRecordEnd (k bs1)
        _                -> tkErr $ "Unexpected " ++ showBeginning bs ++ ", expecting , or }"

    goRK :: Parser TkRecord k
    goRK bs1 k = case Atto.parse jstring_ bs1 of
        Atto.Done bs2 t   -> case uncons bs2 of
            Nothing         -> tkErrEOF ":"
            Just (58, !bs3) -> TkPair t $ goT bs3 $ \bs4 -> goR1 bs4 k
            Just _          -> tkErrBS bs2 ":"
        Atto.Fail _ _ err -> tkErr $ "Error parsing key literal:" ++ err
        Atto.Partial {}   -> tkErr "Unexpected end-of-input while parsing key literal"

    stripPrefix :: ByteString -> Int -> ByteString -> Maybe ByteString
    stripPrefix pfx n bs | BS.isPrefixOf pfx bs = Just (BS.Unsafe.unsafeDrop n bs)
                         | otherwise            = Nothing
    {-# INLINE stripPrefix #-}

    uncons :: ByteString -> Maybe (Word8, ByteString)
    uncons = BS.uncons . skipSpace
    {-# INLINE uncons #-}

type Parser tk k = ByteString -> (ByteString -> k) -> tk k String

showBeginning :: ByteString -> String
showBeginning = show . BS.take 30

-- | Strip leading (ASCII) space
skipSpace :: BS.ByteString -> BS.ByteString
skipSpace bs = case BS.uncons bs of
    Just (w, bs') | w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
        -> skipSpace bs'
    _ -> bs
{-# INLINE skipSpace #-}

-- | Strict bytestring 'Atto.parse' which submits empty string to partial.
attoParse :: Atto.Parser a -> ByteString -> Atto.IResult ByteString a
attoParse p bs = case Atto.parse p bs of
    Atto.Partial k -> k BS.empty
    r              -> r
{-# INLINE attoParse #-}

tkErrEOF :: AsError t =>String ->  t k String
tkErrEOF expected = tkErr $
    "Unexpected end-of-input, expecting " ++ expected

tkErrBS :: AsError t => BS.ByteString -> String ->  t k String
tkErrBS bs expected = tkErr $
    "Unexpected " ++ showBeginning bs ++ ", expecting " ++ expected
