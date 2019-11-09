{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
-- {-# OPTIONS_GHC -fplugin=DumpCore #-}
module Saison.Decoding.Record (
    RecordParser,
    runRecordParser,
    requiredField,
    optionalField,
    skippedField,
    ) where

import Prelude ()
import Prelude.Compat

import Data.Text (Text)

import Saison.Decoding.Result
import Saison.Decoding.Tokens
import Saison.Decoding.Value

data RecordParser a where
    Pure   :: a                                      -> RecordParser a
    Impure :: FieldParser a                          -> RecordParser a
    Ap     :: RecordParser (b -> a) -> FieldParser b -> RecordParser a

instance Functor RecordParser where
    fmap f (Pure x)   = Pure (f x)
    fmap f (Impure x) = Ap (Pure f) x
    fmap f (Ap x y)   = Ap (fmap (f .) x) y

instance Applicative RecordParser where
    pure = Pure

    f <*> Pure x   = Ap f (PureField x)
    f <*> Impure x = Ap f x
    f <*> Ap x y   = Ap ((.) <$> f <*> x) y
    {-# INLINE (<*>) #-}

runRecordParser :: RecordParser a -> Tokens k String -> Result String k a
runRecordParser rp0 (TkRecordOpen rs) = go rp0 rs where
    go :: RecordParser a -> TkRecord k String -> Result String k a
    go _  (TkRecordErr e) = failResult e
    go rp (TkRecordEnd k) = end rp k
    go rp (TkPair t toks) = pair rp t toks >>>= go

    end :: RecordParser a -> k -> Result String k a
    end (Pure x)                     k = pureResult x k
    end (Impure (PureField x))       k = pureResult x k
    end (Impure (OptionalField _ _)) k = pureResult Nothing k
    end (Impure (RequiredField t _)) _ = failResult $ "Field " ++ show t ++ " required"
    end (Ap r (PureField x))         k = ($ x) <$> end r k
    end (Ap r (OptionalField _ _))   k = ($ Nothing) <$> end r k
    end (Ap _ (RequiredField t _))   _ = failResult $ "Field " ++ show t ++ " required"

    pair :: RecordParser a -> Text -> Tokens (TkRecord k String) String -> Result String (TkRecord k String) (RecordParser a)
    pair x@(Pure _)                   _ toks
        -- strict
        -- failResult $ "Unknown field " ++ show t
                     = x <$ skipValue toks

    pair (Ap x (RequiredField s p))   t toks
        | s == t     = Ap x . PureField <$> p toks
    pair (Ap x (OptionalField s p))   t toks
        | s == t     = Ap x . PureField . Just <$> p toks
    pair (Ap x f)                     t toks
                     = (`Ap` f) <$> pair x t toks

    pair (Impure (PureField x))       _ toks =
        Pure x <$ skipValue toks
    pair x@(Impure (OptionalField s p)) t toks
        | s == t     = Impure . PureField . Just <$> p toks
        | otherwise  = x <$ skipValue toks
    pair x@(Impure (RequiredField s p)) t toks
        | s == t     = Impure . PureField <$> p toks
        | otherwise  = x <$ skipValue toks

runRecordParser _  (TkErr e)         = failResult e
runRecordParser _  _                 = failResult "Expecting Record, got ???"

data FieldParser a where
    PureField     :: a                                                        -> FieldParser a
    RequiredField :: Text -> (forall k. Tokens k String -> Result String k a) -> FieldParser a
    OptionalField :: Text -> (forall k. Tokens k String -> Result String k a) -> FieldParser (Maybe a)

requiredField :: Text -> (forall k. Tokens k String -> Result String k a) -> RecordParser a
requiredField n p = Impure (RequiredField n p)

optionalField :: Text -> (forall k. Tokens k String -> Result String k a) -> RecordParser (Maybe a)
optionalField n p = Impure (OptionalField n p)

skippedField :: Text -> RecordParser (a -> a)
skippedField _ = Pure id
