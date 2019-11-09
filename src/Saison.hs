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
    FromTokens (..),
    skipValue,
    -- ** Record
    RecordParser,
    runRecordParser,
    requiredField,
    optionalField,
    skippedField,
    (<.:>),
    (<.:?>),
    -- ** Generic
    genericFromTokensRecord,
    ) where

import Prelude ()
import Prelude.Compat

import Data.ByteString (ByteString)
import Data.Text       (Text)

import qualified Data.ByteString as BS

import Saison.Decoding.Class
import Saison.Decoding.Generic
import Saison.Decoding.Parser
import Saison.Decoding.Record
import Saison.Decoding.Result
import Saison.Decoding.Tokens
import Saison.Decoding.Value

-------------------------------------------------------------------------------
-- Decoding
-------------------------------------------------------------------------------

-- | Parse a value from strict 'ByteString'.
eitherDecodeStrict :: FromTokens a => ByteString -> Either String a
eitherDecodeStrict bs = unResult (fromTokens (tokens bs)) Left $ \x bs' ->
    let bs'' = skipSpace bs'
    in if BS.null bs''
       then Right x
       else Left $ "Unexpected data after the JSON value: " ++ showBeginning bs''

-- | 'requiredField' using 'FromTokens' parser.
(<.:>) :: FromTokens a => RecordParser (a -> b) -> Text -> RecordParser b
p <.:> n = p <*> requiredField n fromTokens

(<.:?>) :: FromTokens a => RecordParser (Maybe a -> b) -> Text -> RecordParser b
p <.:?> n = p <*> optionalField n fromTokens

infixl 4 <.:>, <.:?>
