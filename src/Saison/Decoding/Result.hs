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

(>>>=) :: Result e k a -> (a -> k -> Result e k' b) -> Result e k' b
Result x >>>= y = Result $ \g f -> x g $ \a k -> unResult (y a k) g f

infixl 1 >>>=
