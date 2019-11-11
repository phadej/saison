{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Prelude ()
import Prelude.Compat

import Data.Int                     (Int64)
import Data.Text                    (Text)
import Data.Typeable                (Typeable, typeOf)
import Math.NumberTheory.Logarithms (intLog2)
import Test.QuickCheck              (Arbitrary (..), counterexample, frequency, liftArbitrary, property, sized, (===))
import Test.Tasty                   (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit             (assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck        (testProperty)

import Test.QuickCheck.Instances ()

import qualified Data.Aeson             as Aeson
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import qualified Saison
import qualified Saison.Decoding.Result as Saison

import Saison.Decoding.Examples (Laureate, Laureates, countSomeValues)

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ testCase "laureate.json" $ do
        contents <- BS.readFile "inputs/laureate.json"
        case Aeson.eitherDecodeStrict contents of
            Left err -> assertFailure err
            Right v  -> case Saison.eitherDecodeStrict contents of
                Left err -> assertFailure err
                Right u  -> assertEqual "Laureates" v (u :: Aeson.Value)
    , examples
    , record
    , cornercases
    , agreesWithAeson
    , testProperty "toValue . fromValue = id" $ \v ->
        let rhs = Saison.toValue (Saison.fromValue v)
            lhs = v
        in lhs === rhs
    ]

-------------------------------------------------------------------------------
-- RecordParser
-------------------------------------------------------------------------------

data R = R Text Text
  deriving (Eq, Show)

instance Saison.FromTokens R where
    fromTokens = Saison.runRecordParser $ pure R
        Saison.<.:> "A"
        Saison.<.:> "B"

record :: TestTree
record = testGroup "Record"
    [ testCase "AB" $ do
        let mk :: Text -> Text -> Text -> Text -> Saison.Tokens () String
            mk k x l y = Saison.TkRecordOpen
                $ Saison.TkPair k $ Saison.TkText x
                $ Saison.TkPair l $ Saison.TkText y
                $ Saison.TkPair "C" $ Saison.TkText "Z"
                $ Saison.TkRecordEnd ()

        Saison.unResult (Saison.fromTokens (mk "A" "X" "B" "Y"))
            assertFailure
            $ \r () -> assertEqual "A B" r (R "X" "Y")

        Saison.unResult (Saison.fromTokens (mk "B" "Y" "A" "X"))
            assertFailure
            $ \r () -> assertEqual "B A" r (R "X" "Y")
    ]

-------------------------------------------------------------------------------
-- Examples
-------------------------------------------------------------------------------

examples :: TestTree
examples = testGroup "Examples"
    [ testCase "count laureates" $ do
        contents <- BS.readFile "inputs/laureate.json"
        let n = countSomeValues $ Aeson.eitherDecodeStrict contents
        let y = Saison.eitherDecodeStrict contents
        let m = countSomeValues y
        assertEqual "aeson" n 910
        assertEqual ("saison: " ++ show y) m 910

    , testCase "parse laureates" $ do
        contents <- BS.readFile "inputs/laureate.json"
        case Aeson.eitherDecodeStrict contents of
            Left err -> assertFailure err
            Right x  -> case Saison.eitherDecodeStrict contents of
                Left err -> assertFailure err
                Right y  -> assertEqual "Laureates" x (y :: Laureates Laureate)
    ]

-------------------------------------------------------------------------------
-- Cornercases
-------------------------------------------------------------------------------

cornercases :: TestTree
cornercases = testGroup "cornercases"
    [ testCase "y_array_heterogenous" $
          case Saison.eitherDecodeStrict "[null, 1, \"1\", {}]" :: Either String Aeson.Value of
              Right _  -> return ()
              Left err -> assertFailure err
    ]

-------------------------------------------------------------------------------
-- Agrees with aeson
-------------------------------------------------------------------------------

data P a = P

agreesWithAeson :: TestTree
agreesWithAeson = testGroup "Agrees with aeson"
    [ agrees (P :: P Text)
    , agrees (P :: P [Text])
    , agrees (P :: P Bool)
    , agrees (P :: P Char)
    , agrees (P :: P String) -- [Char]
    , agrees (P :: P [String])
    -- numerals
    , agrees (P :: P Int)
    , agrees (P :: P Int64)
    -- slower
    , agrees (P :: P Aeson.Value)
    ]
  where
    agrees
        :: forall a. (Arbitrary a, Typeable a, Show a, Eq a, Aeson.ToJSON a, Aeson.FromJSON a, Saison.FromTokens a)
        => P a -> TestTree
    agrees _ = testProperty (show (typeOf (undefined :: a))) $ \(z :: a) ->
        let bs = BSL.toStrict (Aeson.encode z)
        in case (Aeson.eitherDecodeStrict bs, Saison.eitherDecodeStrict bs) of
            (Right x, Right y)  -> x === (y :: a)
            (Left _, Left _)    -> property True
            (Left err, Right _) -> counterexample ("aeson failed: " ++ err) False
            (Right _, Left err) -> counterexample ("saison failed: " ++ err) False

-------------------------------------------------------------------------------
-- Orphans
-------------------------------------------------------------------------------

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
