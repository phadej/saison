{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Saison.Decoding.Class (
    FromTokens (..),
    withBool,
    withText,
    withScientific,
    ) where

import Data.Aeson      (Value)
import Data.Int        (Int16, Int32, Int64, Int8)
import Data.Proxy      (Proxy (..))
import Data.Scientific (Scientific)
import Data.Text       (Text)
import Data.Typeable   (Typeable, typeRep)
import Data.Void       (Void)
import Data.Word       (Word, Word16, Word32, Word64, Word8)

import qualified Data.Scientific as Sci
import qualified Data.Text       as T

import Saison.Decoding.Record
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

    fromTokensField :: Text -> RecordParser a
    fromTokensField n = requiredField n fromTokens

    fromTokensFieldList :: Text -> RecordParser [a]
    fromTokensFieldList n = requiredField n fromTokensList

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

withBool :: String -> (Bool -> k -> Result String k a) -> Tokens k String -> Result String k a
withBool _    f (TkLit LitTrue k)  = f True k
withBool _    f (TkLit LitFalse k) = f False k
withBool name _ _                  = Result $ \e _ -> e $ "Expecting bool " ++ name ++ ", got ???"

withText :: String -> (Text -> k -> Result String k a) -> Tokens k String -> Result String k a
withText _    f (TkText t k) = f t k
withText name _ _            = Result $ \e _ -> e $ "Expecting textual " ++ name ++ ", got ???"

withScientific :: String -> (Scientific -> k -> Result String k a) -> Tokens k String -> Result String k a
withScientific _    f (TkNumber t k) = f t k
withScientific name _ _              = Result $ \e _ -> e $ "Expecting number " ++ name ++ ", got ???"

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance FromTokens Bool where
    fromTokens = withBool "Bool" pureResult

instance FromTokens Char where
    fromTokens = withText "Char" $ \t k ->
        if T.length t == 1
        then pureResult (T.head t) k
        else failResult "Expecting single-character string"

    fromTokensList = withText "String" $ \t -> pureResult (T.unpack t)

instance FromTokens a => FromTokens (Maybe a) where
    fromTokens (TkLit LitNull k0) = Result $ \_ f -> f Nothing k0
    fromTokens tokens             = Result $ \g f ->
        unResult (fromTokens tokens) g $ \x tokens' -> f (Just x) tokens'
    fromTokensField n     = optionalField n fromTokens

instance FromTokens a => FromTokens [a] where
    fromTokens      = fromTokensList
    fromTokensField = fromTokensFieldList

-------------------------------------------------------------------------------
-- base numerals
-------------------------------------------------------------------------------

instance FromTokens Int where
    fromTokens = fromTokensBoundedIntegral

instance FromTokens Int8 where
    fromTokens = fromTokensBoundedIntegral

instance FromTokens Int16 where
    fromTokens = fromTokensBoundedIntegral

instance FromTokens Int32 where
    fromTokens = fromTokensBoundedIntegral

instance FromTokens Int64 where
    fromTokens = fromTokensBoundedIntegral

instance FromTokens Word where
    fromTokens = fromTokensBoundedIntegral

instance FromTokens Word8 where
    fromTokens = fromTokensBoundedIntegral

instance FromTokens Word16 where
    fromTokens = fromTokensBoundedIntegral

instance FromTokens Word32 where
    fromTokens = fromTokensBoundedIntegral

instance FromTokens Word64 where
    fromTokens = fromTokensBoundedIntegral

fromTokensBoundedIntegral
    :: forall a k. (Typeable a, Integral a, Bounded a)
    => Tokens k String -> Result String k a
fromTokensBoundedIntegral = withScientific name $ \s k -> maybe
    (failResult $ "value is either floating or will cause over or underflow " ++ show s)
    (`pureResult` k)
    (Sci.toBoundedInteger s)
  where
    name = show (typeRep (Proxy :: Proxy a))

-- parseBoundedIntegralFromScientific :: (Bounded a, Integral a) => Scientific -> Parser a
-- parseBoundedIntegralFromScientific s = maybe
--     (fail $ "value is either floating or will cause over or underflow " ++ show s)
--     pure
--     (Scientific.toBoundedInteger s)
-- {-# INLINE parseBoundedIntegralFromScientific #-}

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
