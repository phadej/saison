{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Math.NumberTheory.Logarithms (intLog2)
import Test.QuickCheck              (Arbitrary (..), frequency, liftArbitrary, sized, (===))
import Test.Tasty                   (defaultMain, testGroup)
import Test.Tasty.HUnit             (assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck        (testProperty)

import Test.QuickCheck.Instances ()

import qualified Data.Aeson      as Aeson
import qualified Data.ByteString as BS
import qualified Saison

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ testCase "laureate.json" $ do
        contents <- BS.readFile "inputs/laureate.json"
        case Aeson.eitherDecodeStrict contents of
            Left err -> assertFailure err
            Right v -> case Saison.eitherDecodeStrict contents of
                Left err -> assertFailure err
                Right u  -> assertEqual "Laureates" v (u :: Aeson.Value)
    , testProperty "toValue . fromValue = id" $ \v ->
        let rhs = Saison.toValue (Saison.fromValue v)
            lhs = v
        in lhs === rhs
    ]

instance Arbitrary Aeson.Value where
    arbitrary = sized arb where
        arb n | n <= 0    = frequency base
              | otherwise = frequency $ base ++
                  [ (1, Aeson.Array `fmap` liftArbitrary (arb $ smaller n))
                  , (1, Aeson.Object `fmap` liftArbitrary (arb $ smaller n))
                  ]

        base =
            [ (1, return Aeson.Null)
            , (3, Aeson.Bool `fmap` arbitrary)
            , (3, Aeson.Number `fmap` arbitrary)
            , (3, Aeson.String `fmap` arbitrary)
            ]

        smaller n | n <= 1    = 0
                  | otherwise = intLog2 n
