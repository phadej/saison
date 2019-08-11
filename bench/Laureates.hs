module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, env, nf)

import qualified Data.Aeson      as Aeson
import qualified Data.ByteString as BS
import qualified Saison

-- Let us read https://easyperf.net/blog/2019/08/02/Perf-measurement-environment-on-Linux
main :: IO ()
main = defaultMain
    [ toValue
    ]
  where
    toValue = env (BS.readFile "inputs/laureate.json") $ \contents ->
        bgroup "ToValue"
            [ bench "Aeson" $ nf (Aeson.eitherDecodeStrict :: BS.ByteString -> Either String Aeson.Value) contents
            , bench "Saison" $ nf (Saison.toEitherValue (fmap show) . Saison.tokens :: BS.ByteString -> Either String Aeson.Value) contents
            ]
