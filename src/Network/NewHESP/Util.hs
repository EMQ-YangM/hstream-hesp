{-# LANGUAGE OverloadedStrings #-}

module Network.NewHESP.Util where

import Criterion.Main (bench, defaultMain, nfIO)
import qualified Data.ByteString as B
import qualified Network.HESP.Protocol as H
import Network.NewHESP.Types (Message (..), decode, encode)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    generate,
    quickCheck,
  )
import Z.Data.Builder
  ( build,
  )
import qualified Z.Data.Parser as P
import qualified Z.Data.Vector as V
import qualified Z.IO.FileSystem as F

prop_cd :: Message -> Bool
prop_cd m =
  let v = build $ encode m
   in case decode v of
        (_, Left _) -> False
        (b, Right v1) -> v1 == m && V.length b == 0

qt = quickCheck prop_cd

test = do
  v <- generate (arbitrary :: Gen Message)
  va <- generate (arbitrary :: Gen Message)
  vb <- generate (arbitrary :: Gen [Message])
  F.quickWriteFile "res" (build $ encode va)
  writeFile "message" (show va)

f = F.quickWriteFile "res" (build $ encode (SimpleString "wellcomaaaaaae"))

fr = do
  v <- F.quickReadFile "res"
  case decode v of
    (_, Left _) -> error "error"
    (_, Right v1) -> return ()

fh = do
  --test
  con <- B.readFile "res"
  case H.deserialize con of
    Left e -> error "error"
    Right i -> return ()

tmain :: IO ()
tmain =
  defaultMain
    [ bench "fr" $ nfIO fr,
      bench "fh" $ nfIO fh
    ]
