{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.NewHESP.Types where

import Control.Monad (forM, replicateM)
import Criterion.Main ()
import Data.Word (Word8)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    frequency,
    sized,
  )
import Z.Data.Builder
  ( Builder,
    bytes,
    char7,
    int,
    integer,
    string7,
  )
import qualified Z.Data.Parser as P
import qualified Z.Data.Vector as V
import Z.Data.Vector.Base (Bytes, Vector, traverseVec_, w2c)
import Z.Data.Vector.FlatMap as M (FlatMap (..), pack, size)

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

encode :: Message -> Builder ()
encode (SimpleString b) = char7 '+' <> bytes b <> sep
encode (BulkString bs) = char7 '$' <> int (V.length bs) <> sep <> bytes bs <> sep
encode (SimpleError et em) = char7 '-' <> bytes et <> char7 ' ' <> bytes em <> sep
encode (Integer i) = char7 ':' <> integer i <> sep
encode (Array vm) = char7 '*' <> int (V.length vm) <> sep <> mapM_ encode vm
encode (Boolean b) = if b then string7 "#t" <> sep else string7 "#f" <> sep
encode (Push bs vm) =
  char7 '>' <> int (V.length vm + 1) <> sep
    <> char7 '$'
    <> int (V.length bs)
    <> sep
    <> bytes bs
    <> sep
    <> mapM_ encode vm
encode (Map m) = do
  char7 '%'
  int (size m)
  sep
  traverseVec_ (\(k, v) -> encode k <> encode v) (sortedKeyValues m)

sep :: Builder ()
sep = "\r\n"

decode = P.parse mParser

mParser :: P.Parser Message
mParser = do
  firstWord <- P.decodePrim @Word8
  case firstWord of
    43 -> pSimpString
    36 -> pBulkString
    45 -> pSimpError
    35 -> pBoolean
    58 -> pInteger
    42 -> pArray
    62 -> pPush
    37 -> pMap
    _ -> fail "strange happened"

pSimpString :: P.Parser Message
pSimpString = do
  v <- P.takeWhile (/= 13)
  eol
  return (SimpleString v)

pBulkString :: P.Parser Message
pBulkString = do
  len <- P.int :: P.Parser Int
  eol
  v <- P.take len
  eol
  return (BulkString v)

pSimpError :: P.Parser Message
pSimpError = do
  et <- P.takeWhile (/= 32)
  P.skipWord8
  em <- P.takeWhile (/= 13)
  eol
  return (SimpleError et em)

pBoolean :: P.Parser Message
pBoolean = do
  v <- P.decodePrim @Word8
  eol
  case v of
    102 -> return $ Boolean False
    116 -> return $ Boolean True
    _ -> fail "boolean value error"

pInteger :: P.Parser Message
pInteger = do
  v <- P.integer
  eol
  return $ Integer v

pArray :: P.Parser Message
pArray = do
  len <- P.int :: P.Parser Int
  eol
  v <- replicateM len mParser
  return (Array (V.pack v))

pPush :: P.Parser Message
pPush = do
  len <- P.int :: P.Parser Int
  eol
  (BulkString h) : t <- replicateM len mParser
  return $ Push h (V.pack t)

pMap :: P.Parser Message
pMap = do
  len <- P.int :: P.Parser Int
  eol
  vs <- replicateM (2 * len) mParser
  let temp = group2 vs
  return $ Map $ M.pack temp

eol :: P.Parser ()
eol = P.char8 '\r' *> P.char8 '\n'

group2 :: [a] -> [(a, a)]
group2 [] = []
group2 (a : b : xs) = (a, b) : group2 xs
group2 _ = error "strange happened"

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