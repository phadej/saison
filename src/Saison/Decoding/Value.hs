{-# LANGUAGE BangPatterns #-}
-- | Convertion to and from @aeson@ 'A.Value'.
module Saison.Decoding.Value (
    toResultValue,
    toEitherValue,
    toValue,
    fromValue,
    skipValue,
    ) where

import Data.Text (Text)
import Data.Void (Void, absurd)

import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V

import Saison.Decoding.Tokens
import Saison.Decoding.Result

-- | Convert 'Tokens' to @aeson's@ 'A.Value'.
--
-- This is an evidence that 'Tokens' encoding is sound.
toValue :: Tokens b Void -> A.Value
toValue = either absurd fst . toEitherValue

-- | Opposite direction of 'toValue'.
fromValue :: A.Value -> Tokens () a
fromValue = go () where
    go :: k -> A.Value -> Tokens k a
    go k A.Null         = TkLit LitNull k
    go k (A.Bool True)  = TkLit LitTrue k
    go k (A.Bool False) = TkLit LitFalse k
    go k (A.String t)   = TkText t k
    go k (A.Number n)   = TkNumber n k
    go k (A.Array xs)   = TkArrayOpen (V.foldr (\v ys -> TkItem (go ys v)) (TkArrayEnd k) xs)
    go k (A.Object xs)  = TkRecordOpen (HM.foldrWithKey (\i v ys -> TkPair i (go ys v)) (TkRecordEnd k) xs)

toEitherValue
    :: Tokens k e             -- ^ tokens
    -> Either e (A.Value, k)  -- ^ either token error or value and leftover.
toEitherValue t = unResult (toResultValue t) Left $ \v k -> Right (v, k)

-- | Convert to 'Value', from 'Tokens' potentially containing an error.
-- Also option to check the left over.
toResultValue
    :: Tokens k e           -- ^ tokens
    -> Result e k A.Value  -- ^ either token error or value and leftover.
toResultValue t0 = Result (go t0) where
    go :: Tokens k e -> (e -> r) -> (A.Value -> k -> r) -> r
    go (TkLit LitNull k)  _ f = f A.Null k
    go (TkLit LitTrue k)  _ f = f (A.Bool True) k
    go (TkLit LitFalse k) _ f = f (A.Bool False) k
    go (TkText t k)       _ f = f (A.String t) k
    go (TkNumber n k)     _ f = f (A.Number n) k
    go (TkArrayOpen arr)  g f = goA 0 id arr g $ \n xs k -> f (A.Array (V.fromListN n xs)) k
    go (TkRecordOpen rec) g f = goR id rec g $ \xs k -> f (A.Object (HM.fromList xs)) k
    go (TkErr e)          g _ = g e

    goA :: Int                           -- size accumulator
        -> ([A.Value] -> [A.Value])      -- dlist accumulator
        -> TkArray k e                   -- array tokens
        -> (e -> r)                      -- error continuation
        -> (Int -> [A.Value] -> k -> r)  -- success continuation
        -> r
    goA !n !acc (TkItem toks)  g f = go toks g $ \v k -> goA (succ n) (acc . (v :)) k g f
    goA !n !acc (TkArrayEnd k) _ f = f n (acc []) k
    goA !_ !_   (TkArrayErr e) g _ = g e

    goR :: ([(Text, A.Value)] -> [(Text, A.Value)])
        -> TkRecord k e
        -> (e -> r)
        -> ([(Text, A.Value)] -> k -> r)
        -> r
    goR !acc (TkPair t toks) g f = go toks g $ \v k -> goR (acc . ((t, v) :)) k g f
    goR !acc (TkRecordEnd k) _ f = f (acc []) k
    goR !_   (TkRecordErr e) g _ = g e

-- | Skip value. Useful sometimes.
skipValue
    :: Tokens k e     -- ^ tokens
    -> Result e k ()
skipValue t0 = Result $ \g f -> go t0 g $ \k -> f () k where
    go :: Tokens k e -> (e -> r) -> (k -> r) -> r
    go (TkLit _ k)        _ f = f k
    go (TkText _ k)       _ f = f k
    go (TkNumber _ k)     _ f = f k
    go (TkArrayOpen arr)  g f = goA arr g f
    go (TkRecordOpen rec) g f = goR rec g f
    go (TkErr e)          g _ = g e

    goA :: TkArray k e                   -- array tokens
        -> (e -> r)                      -- error continuation
        -> (k -> r)  -- success continuation
        -> r
    goA (TkItem toks)  g f = go toks g $ \k -> goA k g f
    goA (TkArrayEnd k) _ f = f k
    goA (TkArrayErr e) g _ = g e

    goR :: TkRecord k e
        -> (e -> r)
        -> (k -> r)
        -> r
    goR (TkPair _ toks) g f = go toks g $ \k -> goR k g f
    goR (TkRecordEnd k) _ f = f k
    goR (TkRecordErr e) g _ = g e


