{-# LANGUAGE OverloadedStrings #-}
-- | Various examples of using @saison@.
module Saison.Decoding.Examples (
    -- * SomeValue
    SomeValue (..),
    countSomeValues,
    -- * Laureates
    Laureates (..),
    ) where

import Prelude ()
import Prelude.Compat
import Control.DeepSeq (NFData (..))

import qualified Data.Aeson as Aeson

import Saison
import Saison.Decoding.Result

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
  deriving (Show)

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
    fromTokens (TkRecordOpen toks0) = go toks0 where
        go (TkPair t toks1)
            | t == "laureates" =
                Result $ \g f ->
                unResult (fromTokens toks1) g $ \xs k -> case k of
                    TkRecordEnd k' -> f (Laureates xs) k'
                    _              -> g "Expecting record with exactly one key"
            | otherwise        = failResult $ "Expecting laureates key, got " ++ show t
        go (TkRecordErr e) = failResult e
        go (TkRecordEnd _) = failResult "Expecting record with exactly one key"
    fromTokens (TkErr e)   = failResult e
    fromTokens _           = failResult "Expecting Record, got ????"
