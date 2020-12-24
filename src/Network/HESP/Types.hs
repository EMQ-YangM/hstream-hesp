{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.HESP.Types (Message (..)) where

import Control.Monad (forM)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    frequency,
    sized,
  )
import qualified Z.Data.Vector as V
import Z.Data.Vector.Base (Bytes, Vector, w2c)
import Z.Data.Vector.FlatMap as M (FlatMap (..), pack)

-- | redis protocol message,
-- In RESP different parts of the protocol are always terminated with "\r\n" (CRLF).
data Message
  = -- | Simple String  first byte is "+"
    SimpleString Bytes
  | -- | Bulk String first byte is "$"
    BulkString Bytes
  | -- | Simple Error first byte is "-"
    SimpleError Bytes Bytes
  | -- | Integers first bytes is ":"
    Integer Integer
  | -- | Array first bytes is "*"
    Array (Vector Message)
  | -- | Boolean first byte is "#"
    Boolean Bool
  | -- | Push first byte is ">"
    Push Bytes (Vector Message)
  | -- | Map first byte is "%"
    Map (FlatMap Message Message)
  deriving (Eq, Ord, Show)

-- | Arbitrary Message
-- n `div` 4
instance Arbitrary Message where
  arbitrary = sized createMessage
    where
      createMessage n
        | n > 0 =
          frequency
            [ (1, SimpleString <$> fmap filter2 arbitrary),
              (1, BulkString <$> fmap filter2 arbitrary),
              (1, SimpleError <$> fmap filter3 arbitrary <*> fmap filter3 arbitrary),
              (1, Integer <$> arbitrary),
              (1, Array <$> clist),
              (1, Boolean <$> arbitrary),
              (1, Push <$> arbitrary <*> clist),
              (1, Map <$> cmap)
            ]
        | n <= 0 = f
        where
          clist = do
            let t = n `div` 4
            forM (V.pack [1 .. n]) (\_ -> createMessage t)
          cmap = do
            let t = n `div` 4
            v <- forM [1 .. n] $ \_ -> do
              k <- f
              val <- createMessage t
              return (k, val)
            return $ M.pack v
      createMessage _ = error "strange errpr"
      f =
        frequency
          [ (1, SimpleString <$> fmap filter2 arbitrary),
            (1, BulkString <$> fmap filter2 arbitrary),
            (1, SimpleError <$> fmap filter3 arbitrary <*> fmap filter3 arbitrary),
            (1, Integer <$> arbitrary),
            (1, Boolean <$> arbitrary)
          ]
      filter2 = V.filter ((`Prelude.notElem` ("\r\n" :: String)) . w2c)
      filter3 = V.filter ((`Prelude.notElem` (" \r\n" :: String)) . w2c)