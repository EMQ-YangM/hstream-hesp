{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (void)
import Network.HESP.Protocol (decode, decodes, encode)
import Network.HESP.Types (Message (..))
import Test.QuickCheck.All (quickCheckAll)
import Z.Data.Builder
  ( build,
  )
import qualified Z.Data.Vector as V

prop_msgEncodeDecode :: Message -> Bool
prop_msgEncodeDecode m =
  let v = build $ encode m
   in case decode v of
        (_, Left _) -> False
        (b, Right v1) -> v1 == m && V.length b == 0

prop_msgsEncodeDecode :: [Message] -> Bool
prop_msgsEncodeDecode ms =
  let v = build $ traverse encode ms
   in case decodes v of
        (_, Left _) -> False
        (b, Right v1) -> v1 == ms && V.length b == 0

return []

main :: IO ()
main = void $ $quickCheckAll
