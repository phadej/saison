module Main (main) where

import qualified Saison

import Saison.JSONTestSuite (jsonTestSuite)

main :: IO ()
main = jsonTestSuite Saison.eitherDecodeStrict
