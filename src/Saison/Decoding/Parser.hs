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
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Unsafe     as BS.Unsafe

import Saison.Decoding.Tokens

-- | Lex (and parse) the 'ByteString' into 'Tokens' stream.
--
-- The rest of input is in @'Maybe' 'ByteString'@. Errors are simple textual errors atm.
--
-- * TODO: introduce abstraction for @Parser tok k = BS -> (BS -> k) -> tok k String@
-- * TODO: try with stack of parsers, and not making new continuations. Will it be faster?
--
tokens :: ByteString -> Tokens ByteString String
tokens bs0 = goT bs0 id where
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
