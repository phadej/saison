{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Saison.Decoding.Class where

import Data.Aeson (Value)
import Data.Text  (Text)
import Data.Void  (Void)

import qualified Data.Text as T

import Saison.Decoding.Result
import Saison.Decoding.Tokens
import Saison.Decoding.Value

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

class FromTokens a where
    fromTokens :: Tokens k String -> Result String k a

    fromTokensList :: Tokens k String -> Result String k [a]
    fromTokensList (TkArrayOpen toks0) = Result $ \g f -> goA id toks0 g f where
        goA :: ([a] -> [a]) -> TkArray k String -> (String -> r) -> ([a] -> k -> r) -> r
        goA !acc (TkItem toks)  g f =
            unResult (fromTokens toks) g $ \x toks' -> goA (acc . (x :)) toks' g f
        goA !acc (TkArrayEnd k) _ f = f (acc []) k
        goA _    (TkArrayErr e) g _ = g e

    fromTokensList (TkErr err) = failResult err
    fromTokensList _ = failResult "Expecting array, got ???"

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

withText :: String -> (Text -> k -> Result String k a) -> Tokens k String -> Result String k a
withText _    f (TkText t k) = f t k
withText name _ _            = Result $ \e _ -> e $ "Expecting textual " ++ name ++ ", got ???"

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance FromTokens Char where
    fromTokens = withText "Char" $ \t k ->
        if T.length t == 1
        then pureResult (T.head t) k
        else failResult "Expecting single-character string"

    fromTokensList = withText "String" $ \t -> pureResult (T.unpack t)

instance FromTokens a => FromTokens [a] where
    fromTokens = fromTokensList

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

instance FromTokens Value where
    fromTokens = toResultValue

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

-- TODO

-------------------------------------------------------------------------------
-- nats
-------------------------------------------------------------------------------

-- TODO

-------------------------------------------------------------------------------
-- semigroups
-------------------------------------------------------------------------------

-- TODO

-------------------------------------------------------------------------------
-- scientific
-------------------------------------------------------------------------------

-- TODO

---------------------------------------------------------------------------------
-- text
-------------------------------------------------------------------------------

instance FromTokens Text where
    fromTokens = withText "Text" pureResult

-------------------------------------------------------------------------------
-- these
-------------------------------------------------------------------------------

-- TODO

-------------------------------------------------------------------------------
-- uuid-types
-------------------------------------------------------------------------------

-- TODO

-------------------------------------------------------------------------------
-- vector
-------------------------------------------------------------------------------

-- TODO

-------------------------------------------------------------------------------
-- void
-------------------------------------------------------------------------------

instance FromTokens Void where
    fromTokens _ = failResult "Void cannot be constructed"
