module Main (main) where

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, env, whnf, nf)

import qualified Data.Aeson      as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Saison

import Saison.Decoding.Examples (countSomeValues)

main :: IO ()
main = defaultMain
    [ value
    , count
    ]
  where
    value :: Benchmark
    value =
        env (BS.readFile "inputs/laureate.json") $ \bs ->
        env (LBS.readFile "inputs/laureate.json") $ \lbs ->
        bgroup "Value"
            [ bench "Aeson"       $ nf (Aeson.eitherDecodeStrict  :: BS.ByteString  -> Either String Aeson.Value) bs
            , bench "Aeson'"      $ nf (Aeson.eitherDecodeStrict' :: BS.ByteString  -> Either String Aeson.Value) bs
            , bench "Aeson Lazy"  $ nf (Aeson.eitherDecode        :: LBS.ByteString -> Either String Aeson.Value) lbs
            , bench "Aeson' Lazy" $ nf (Aeson.eitherDecode'       :: LBS.ByteString -> Either String Aeson.Value) lbs
            , bench "Saison"      $ nf (Saison.eitherDecodeStrict :: BS.ByteString  -> Either String Aeson.Value) bs
            ]

    count :: Benchmark
    count =
        env (BS.readFile "inputs/laureate.json") $ \bs ->
        env (LBS.readFile "inputs/laureate.json") $ \lbs ->
        bgroup "Count"
            [ bench "Aeson"       $ whnf (countSomeValues . Aeson.eitherDecodeStrict ) bs
            , bench "Aeson'"      $ whnf (countSomeValues . Aeson.eitherDecodeStrict') bs
            , bench "Aeson Lazy"  $ whnf (countSomeValues . Aeson.eitherDecode       ) lbs
            , bench "Aeson' Lazy" $ whnf (countSomeValues . Aeson.eitherDecode'      ) lbs
            , bench "Saison"      $ whnf (countSomeValues . Saison.eitherDecodeStrict) bs
            ]
