module Main (main) where

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, env, nf, whnf)

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Saison

import Saison.Decoding.Examples (Laureate, Laureates, countSomeValues)

type V = Either String Aeson.Value
type L = Either String (Laureates Laureate)

main :: IO ()
main = defaultMain
    [ value
    , count
    , parse
    ]
  where
    value :: Benchmark
    value =
        env (BS.readFile "inputs/laureate.json") $ \bs ->
        env (LBS.readFile "inputs/laureate.json") $ \lbs ->
        bgroup "Value"
            [ bench "Aeson"       $ nf (Aeson.eitherDecodeStrict  :: BS.ByteString  -> V) bs
            , bench "Aeson'"      $ nf (Aeson.eitherDecodeStrict' :: BS.ByteString  -> V) bs
            , bench "Aeson Lazy"  $ nf (Aeson.eitherDecode        :: LBS.ByteString -> V) lbs
            , bench "Aeson' Lazy" $ nf (Aeson.eitherDecode'       :: LBS.ByteString -> V) lbs
            , bench "Saison"      $ nf (Saison.eitherDecodeStrict :: BS.ByteString  -> V) bs
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

    parse :: Benchmark
    parse =
        env (BS.readFile "inputs/laureate.json") $ \bs ->
        env (LBS.readFile "inputs/laureate.json") $ \lbs ->
        bgroup "Parse"
            [ bench "Aeson"       $ nf (Aeson.eitherDecodeStrict  :: BS.ByteString  -> L) bs
            , bench "Aeson'"      $ nf (Aeson.eitherDecodeStrict' :: BS.ByteString  -> L) bs
            , bench "Aeson Lazy"  $ nf (Aeson.eitherDecode        :: LBS.ByteString -> L) lbs
            , bench "Aeson' Lazy" $ nf (Aeson.eitherDecode'       :: LBS.ByteString -> L) lbs
            , bench "Saison"      $ nf (Saison.eitherDecodeStrict :: BS.ByteString  -> L) bs
            ]

