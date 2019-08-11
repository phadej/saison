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
    eitherDecodeStrict,
    toEitherValue,
    tokens,
    ) where

import Data.Aeson      (Value)
import Data.ByteString (ByteString)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8

import Saison.Decoding.Parser
import Saison.Decoding.Tokens
import Saison.Decoding.Value

-------------------------------------------------------------------------------
-- Decoding
-------------------------------------------------------------------------------

class FromTokens a where
    fromTokens :: Tokens k String -> Either String (a, k)

instance FromTokens Value where
    fromTokens = toEitherValue

-- |
--
-- * TODO: Make 'FromTokens' type class
eitherDecodeStrict :: FromTokens a => ByteString -> Either String a
eitherDecodeStrict bs = case fromTokens (tokens bs) of
    Left err                                 -> Left err
    Right (x, bs') | BS.null (skipSpace bs') -> Right x
                   | otherwise               -> Left $ "Unexpected data after the JSON value: " ++ BS8.unpack (BS.take 30 bs)
