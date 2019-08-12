module Main (main) where

import qualified Data.Aeson as Aeson

import Saison.JSONTestSuite (jsonTestSuite)

main :: IO ()
main = jsonTestSuite Aeson.eitherDecodeStrict
