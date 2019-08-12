{-# LANGUAGE RankNTypes #-}
module Saison.Decoding.Result where

-- | /TODO/ I'm not sure this is the type we want.
--
-- Maybe we want bundle input into this, and make this class
-- "Profunctor-y".
--
newtype Result e k a = Result
    { unResult :: forall r. (e -> r) -> (a -> k -> r) -> r }

instance Functor (Result e k) where
    fmap h (Result k) = Result $ \g f -> k g $ \x -> f (h x)

pureResult :: a -> k -> Result e k a
pureResult x k = Result $ \_ f -> f x k

failResult :: e -> Result e k a
failResult e = Result $ \g _ -> g e
