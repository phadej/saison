{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Saison.Decoding.Generic (
    -- * Record
    genericFromTokensRecord,
    GFromTokensRecord,
    ) where

import Prelude ()
import Prelude.Compat

import Data.Functor.Confusing (CurriedYoneda, liftCurriedYoneda, lowerCurriedYoneda)
import Data.Text              (pack)
import GHC.Generics           ((:*:) (..), C, D, Generic (..), K1 (..), M1 (..), S, Selector (..))

import Saison.Decoding.Class
import Saison.Decoding.Record
import Saison.Decoding.Result
import Saison.Decoding.Tokens

-------------------------------------------------------------------------------
-- Record
-------------------------------------------------------------------------------

-- | Generally decode record types using 'fromTokensField' for individual
-- fields.

genericFromTokensRecord
    :: (Generic a, GFromTokensRecord (Rep a))
    => (String -> String)  -- ^ field renamer
    -> Tokens k String
    -> Result String k a
genericFromTokensRecord fieldNamer = \tokens ->
    runRecordParser (lowerCurriedYoneda (to <$> grecord fieldNamer)) tokens
{-# INLINE genericFromTokensRecord #-}

class GFromTokensRecord f where
    grecord :: (String -> String) -> CurriedYoneda RecordParser (f a)

instance (GRecord1 f, i ~ D) => GFromTokensRecord (M1 i c f) where
    grecord namer = M1 <$> grecord1 namer
    {-# INLINE grecord #-}

class GRecord1 f where
    grecord1 :: (String -> String) -> CurriedYoneda RecordParser (f a)

instance (GRecord2 f, i ~ C) => GRecord1 (M1 i c f) where
    grecord1 namer = M1 <$> grecord2 namer
    {-# INLINE grecord1 #-}

class GRecord2 f where
    grecord2 :: (String -> String) -> CurriedYoneda RecordParser (f a)

instance (GRecord2 f, GRecord2 g) => GRecord2 (f :*: g) where
    grecord2 namer = (:*:) <$>  grecord2 namer <*> grecord2 namer
    {-# INLINE grecord2 #-}

instance (GRecord3 f, Selector c, i ~ S) => GRecord2 (M1 i c f) where
    grecord2 namer =  fmap M1 (grecord3 (namer $ selName (undefined :: M1 i c f ())))
    {-# INLINE grecord2 #-}

class GRecord3 f where
    grecord3 :: String -> CurriedYoneda RecordParser (f a)

instance FromTokens c => GRecord3 (K1 i c) where
    grecord3 name = K1 <$> liftCurriedYoneda (fromTokensField (pack name))
    {-# INLINE grecord3 #-}
