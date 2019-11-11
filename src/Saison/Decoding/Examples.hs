{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -fplugin=DumpCore #-}
-- {-# OPTIONS_GHC -funfolding-use-threshold=200 #-}

-- | Various examples of using @saison@.
module Saison.Decoding.Examples (
    -- * SomeValue
    SomeValue (..),
    countSomeValues,
    -- * Laureates
    Laureates (..),
    Laureate (..),
    ) where

import Prelude ()
import Prelude.Compat

import Control.DeepSeq (NFData (..))
import Data.Char       (toLower)
import Data.Text       (Text)
import GHC.Generics    (Generic)

import qualified Data.Aeson as Aeson

import Saison

-------------------------------------------------------------------------------
-- SomeValue
-------------------------------------------------------------------------------

-- | 'SomeValue' is a dummy object.
-- Decoding it from a JSON value always succeeds,
-- but we don't preserve any information about that value.
--
-- If we decode a list @['SomeValue']@, we can then 'count' values in it.
--
-- This is an extreme example where @saison@ approach shines.
--
-- @
-- instance 'FromTokens' 'SomeValue' where
--     'fromTokens' = \\toks -> 'SomeValue' '<$' 'skipValue' toks
-- @
data SomeValue = SomeValue
  deriving (Eq, Ord, Show)

instance NFData SomeValue where
    rnf SomeValue = ()

instance Aeson.FromJSON SomeValue where
    parseJSON _ = return SomeValue

instance Aeson.ToJSON SomeValue where
    toJSON _ = Aeson.Null

instance FromTokens SomeValue where
    fromTokens = \toks -> SomeValue <$ skipValue toks

-- | Count 'SomeValue's.
countSomeValues :: Either e (Laureates SomeValue) -> Int
countSomeValues = either (const (-1)) (length . unLaureates)

-------------------------------------------------------------------------------
-- Laureates
-------------------------------------------------------------------------------

-- | @'Laureates' a@ is a wrapper around a list of elements.
--
-- The @laureate.json@ database is a json of form
--
-- @
-- {
--   "laureates": [
--      ...
--   ]
-- }
-- @
--
-- This type parses the outer layer.
--
newtype Laureates a = Laureates { unLaureates :: [a] }
  deriving (Eq, Show)

instance NFData a => NFData (Laureates a) where
    rnf (Laureates xs) = rnf xs

instance Aeson.FromJSON a => Aeson.FromJSON (Laureates a) where
    parseJSON = Aeson.withObject "Laureates" $ \obj ->
        Laureates <$> obj Aeson..: "laureates"

-- | This instance differs from @aeson@'s,
-- here we require that object has exactly one key: @"laureates"@.
--
-- * TODO: currently this instance is written manually.
--   Separate ways to interpret record: lenient and strict.
--
instance FromTokens a => FromTokens (Laureates a) where
    fromTokens = runRecordParser $ pure Laureates <.:> "laureates"

-------------------------------------------------------------------------------
-- Laureate
-------------------------------------------------------------------------------

-- | Almost complete information about a laureate. We skip @"prizes"@ information.
--
-- Look at the implementaton of 'FromJSON' and 'FromTokens' instances,
-- they don't look that much different.
--
data Laureate = Laureate
    { lBorn            :: !Text -- change to Day
    , lBornCity        :: !(Maybe Text)
    , lBornCountry     :: !(Maybe Text)
    , lBornCountryCode :: !(Maybe Text)
    , lDied            :: !Text -- change to Day
    , lDiedCity        :: !(Maybe Text)
    , lDiedCountry     :: !(Maybe Text)
    , lDiedCountryCode :: !(Maybe Text)
    , lFirstname       :: !(Maybe Text)
    , lSurname         :: !(Maybe Text)
    , lId              :: !Text
    , lGender          :: !Text
    }
  deriving (Eq, Show, Generic)

instance NFData Laureate

instance Aeson.FromJSON Laureate where
    parseJSON = Aeson.withObject "Laureate" $ \obj -> Laureate
        <$> obj Aeson..:  "born"
        <*> obj Aeson..:? "bornCity"
        <*> obj Aeson..:? "bornCountry"
        <*> obj Aeson..:? "bornCountryCode"
        <*> obj Aeson..:  "died"
        <*> obj Aeson..:? "diedCity"
        <*> obj Aeson..:? "diedCountry"
        <*> obj Aeson..:? "diedCountryCode"
        <*> obj Aeson..:? "firstname"
        <*> obj Aeson..:? "surname"
        <*> obj Aeson..:  "id"
        <*> obj Aeson..:  "gender"

instance FromTokens Laureate where
    fromTokens = genericFromTokensRecord renamer where
        renamer (_:x:xs) = toLower x : xs
        renamer xs       = xs
{-
    fromTokens = runRecordParser $ pure Laureate
        <.:>  "born"
        <.:?> "bornCity"
        <.:?> "bornCountry"
        <.:?> "bornCountryCode"
        <.:>  "died"
        <.:?> "diedCity"
        <.:?> "diedCountry"
        <.:?> "diedCountryCode"
        <.:?> "firstname"
        <.:?> "surname"
        <.:>  "id"
        <.:>  "gender"
-}
