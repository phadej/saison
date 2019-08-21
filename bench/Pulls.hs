{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude ()
import Prelude.Compat

import Control.DeepSeq (NFData (..))
import Criterion.Main  (Benchmark, bench, bgroup, defaultMain, env, nf, whnf)
import Data.Text       (Text)

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Saison

import Saison.Decoding.Examples (SomeValue)

data Pull = Pull
    { _pullNodeId :: !Text
    , _pullTitle  :: !Text
    }
  deriving (Eq, Ord)

instance NFData Pull where
    rnf Pull {} = ()

instance Aeson.FromJSON Pull where
    parseJSON = Aeson.withObject "Pull" $ \obj -> Pull
        <$> obj Aeson..: "node_id"
        <*> obj Aeson..: "title"

instance Saison.FromTokens Pull where
    fromTokens = Saison.runRecordParser $ pure Pull
        Saison.<.:> "node_id"
        Saison.<.:> "title"

type V  = Either String Aeson.Value
type L = Either String [Aeson.Value]
type X  = Either String [Pull]

countSomeValues :: Either e [SomeValue] -> Int
countSomeValues = either (const (-1)) length

main :: IO ()
main = defaultMain
    [ value
    , count
    , listvalue
    , parse
    ]
  where
    value :: Benchmark
    value =
        env (BS.readFile "inputs/pulls.json") $ \bs ->
        env (LBS.readFile "inputs/pulls.json") $ \lbs ->
        bgroup "Value"
            [ bench "Aeson"       $ nf (Aeson.eitherDecodeStrict  :: BS.ByteString  -> V) bs
            , bench "Aeson'"      $ nf (Aeson.eitherDecodeStrict' :: BS.ByteString  -> V) bs
            , bench "Aeson Lazy"  $ nf (Aeson.eitherDecode        :: LBS.ByteString -> V) lbs
            , bench "Aeson' Lazy" $ nf (Aeson.eitherDecode'       :: LBS.ByteString -> V) lbs
            , bench "Saison"      $ nf (Saison.eitherDecodeStrict :: BS.ByteString  -> V) bs
            , bench "Saison2"      $ nf (Saison.eitherDecodeStrict2 :: BS.ByteString  -> V) bs
            ]

    count :: Benchmark
    count =
        env (BS.readFile "inputs/pulls.json") $ \bs ->
        env (LBS.readFile "inputs/pulls.json") $ \lbs ->
        bgroup "Count"
            [ bench "Aeson"       $ whnf (countSomeValues . Aeson.eitherDecodeStrict ) bs
            , bench "Aeson'"      $ whnf (countSomeValues . Aeson.eitherDecodeStrict') bs
            , bench "Aeson Lazy"  $ whnf (countSomeValues . Aeson.eitherDecode       ) lbs
            , bench "Aeson' Lazy" $ whnf (countSomeValues . Aeson.eitherDecode'      ) lbs
            , bench "Saison"      $ whnf (countSomeValues . Saison.eitherDecodeStrict) bs
            , bench "Saison2"      $ whnf (countSomeValues . Saison.eitherDecodeStrict2) bs
            ]

    listvalue :: Benchmark
    listvalue =
        env (BS.readFile "inputs/pulls.json") $ \bs ->
        env (LBS.readFile "inputs/pulls.json") $ \lbs ->
        bgroup "ListValue"
            [ bench "Aeson"       $ nf (Aeson.eitherDecodeStrict  :: BS.ByteString  -> L) bs
            , bench "Aeson'"      $ nf (Aeson.eitherDecodeStrict' :: BS.ByteString  -> L) bs
            , bench "Aeson Lazy"  $ nf (Aeson.eitherDecode        :: LBS.ByteString -> L) lbs
            , bench "Aeson' Lazy" $ nf (Aeson.eitherDecode'       :: LBS.ByteString -> L) lbs
            , bench "Saison"      $ nf (Saison.eitherDecodeStrict :: BS.ByteString  -> L) bs
            , bench "Saison2"      $ nf (Saison.eitherDecodeStrict2 :: BS.ByteString  -> L) bs
            ]

    parse :: Benchmark
    parse =
        env (BS.readFile "inputs/pulls.json") $ \bs ->
        env (LBS.readFile "inputs/pulls.json") $ \lbs ->
        bgroup "Parse"
            [ bench "Aeson"       $ nf (Aeson.eitherDecodeStrict  :: BS.ByteString  -> X) bs
            , bench "Aeson'"      $ nf (Aeson.eitherDecodeStrict' :: BS.ByteString  -> X) bs
            , bench "Aeson Lazy"  $ nf (Aeson.eitherDecode        :: LBS.ByteString -> X) lbs
            , bench "Aeson' Lazy" $ nf (Aeson.eitherDecode'       :: LBS.ByteString -> X) lbs
            , bench "Saison"      $ nf (Saison.eitherDecodeStrict :: BS.ByteString  -> X) bs
            , bench "Saison2"      $ nf (Saison.eitherDecodeStrict2 :: BS.ByteString  -> X) bs
            ]

