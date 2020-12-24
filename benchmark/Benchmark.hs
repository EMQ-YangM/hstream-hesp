{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main (bench, defaultMain, nfIO, whnf)
import Network.HESP.Protocol (decode, encode)
import Network.HESP.Types (Message (..))
import Z.Data.Builder
  ( build,
  )
import qualified Z.IO.FileSystem as F

rdmessage_ :: IO ()
rdmessage_ = do
  v <- F.quickReadFile "benchmark/redis"
  case decode v of
    (_, Left _) -> error "error"
    (_, Right _) -> return ()

rdmessage :: IO Message
rdmessage = do
  v <- F.quickReadFile "benchmark/redis"
  case decode v of
    (_, Left _) -> error "error"
    (_, Right v1) -> return v1

main :: IO ()
main = do
  v <- rdmessage
  defaultMain
    [ bench "read and decode redis file (435.9k)" (nfIO rdmessage_),
      bench "encode redis file (435.9k)" $ whnf (build . encode) v
    ]
