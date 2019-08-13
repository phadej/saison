{-# LANGUAGE RankNTypes #-}
module Saison.Decoding.Record (
    RecordParser,
    runRecordParser,
    requiredField,
    optionalField,
    skippedField,
    ) where

import Prelude ()
import Prelude.Compat

import Data.Set  (Set)
import Data.Text (Text)

import qualified Data.Primitive.SmallArray as SA

import Saison.Decoding.Result
import Saison.Decoding.Tokens

-- TODO: implement me
data RecordParser a = RecordParser

instance Functor RecordParser where
    fmap f x = pure f <*> x

instance Applicative RecordParser where
    pure _ = error "pure @RecordParser" RecordParser
    (<*>) = error "ap @RecordParser"

runRecordParser :: RecordParser a -> Tokens k String -> Result String k a
runRecordParser = error "implement me"

requiredField :: Text -> (forall k. Tokens k String -> Result String k a) -> RecordParser a
requiredField _n _p = undefined

optionalField :: Text -> (forall k. Tokens k String -> Result String k a) -> RecordParser (Maybe a)
optionalField _n _p = undefined

skippedField :: Text -> RecordParser (a -> a)
skippedField = undefined
