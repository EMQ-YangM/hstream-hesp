{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.NewHESP.Types where

import Control.Monad (forM, replicateM)
import Criterion.Main
import qualified Data.ByteString as B
import Data.Word (Word8)
import qualified Network.HESP.Protocol as H
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    frequency,
    generate,
    quickCheck,
    sized,
  )
import Z.Data.Builder
  ( Builder,
    build,
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
import qualified Z.IO.FileSystem as F

-- | redis protocol message,
-- In RESP different parts of the protocol are always terminated with "\r\n" (CRLF).
data Message
  = -- | Simple String  first byte is "+"
    SimpleString {-# UNPACK #-} !Bytes
  | -- | Bulk String first byte is "$"
    BulkString {-# UNPACK #-} !Bytes
  | -- | Simple Error first byte is "-"
    SimpleError {-# UNPACK #-} !Bytes !Bytes
  | -- | Integers first bytes is ":"
    Integer {-# UNPACK #-} !Integer
  | -- | Array first bytes is "*"
    Array (Vector Message)
  | -- | Boolean first byte is "#"
    Boolean {-# UNPACK #-} !Bool
  | -- | Push first byte is ">"
    Push Bytes (Vector Message)
  | -- | Map first byte is "%"
    Map (FlatMap Message Message)
  deriving (Eq, Ord, Show)

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
            let t = n `div` 2
            forM (V.pack [1 .. n]) (\_ -> createMessage t)
          cmap = do
            let t = n `div` 2
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

decode :: P.Parser Message
decode = do
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
    _ -> error "strange happened"

pSimpString :: P.Parser Message
pSimpString = do
  v <- P.takeWhile (/= 13)
  eol
  return (SimpleString v)
{-# INLINE pSimpString #-}

pBulkString :: P.Parser Message
pBulkString = do
  len <- P.int :: P.Parser Int
  eol
  v <- P.take len
  eol
  return (BulkString v)
{-# INLINE pBulkString #-}

pSimpError :: P.Parser Message
pSimpError = do
  et <- P.takeWhile (/= 32)
  P.skipWord8
  em <- P.takeWhile (/= 13)
  eol
  return (SimpleError et em)
{-# INLINE pSimpError #-}

pBoolean :: P.Parser Message
pBoolean = do
  v <- P.decodePrim @Word8
  eol
  case v of
    102 -> return $ Boolean False
    116 -> return $ Boolean True
    _ -> error "boolean value error"
{-# INLINE pBoolean #-}

pInteger :: P.Parser Message
pInteger = do
  v <- P.integer
  eol
  return $ Integer v
{-# INLINE pInteger #-}

pArray :: P.Parser Message
pArray = do
  len <- P.int :: P.Parser Int
  eol
  v <- replicateM len decode
  return (Array (V.pack v))
{-# INLINE pArray #-}

pPush :: P.Parser Message
pPush = do
  len <- P.int :: P.Parser Int
  eol
  (BulkString h) : t <- replicateM len decode
  return $ Push h (V.pack t)
{-# INLINE pPush #-}

pMap :: P.Parser Message
pMap = do
  len <- P.int :: P.Parser Int
  eol
  vs <- replicateM (2 * len) decode
  let temp = group2 vs
  return $ Map $ M.pack temp
{-# INLINE pMap #-}

eol :: P.Parser ()
eol = P.char8 '\r' *> P.char8 '\n'
{-# INLINE eol #-}

group2 :: [a] -> [(a, a)]
group2 [] = []
group2 (a : b : xs) = (a, b) : group2 xs
group2 _ = error "strange happened"

prop_cd :: Message -> Bool
prop_cd m =
  let v = build $ encode m
   in case P.parse decode v of
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
  case P.parse decode v of
    (_, Left _) -> error "error"
    (_, Right v1) -> return ()

fh = do
  --test
  con <- B.readFile "res"
  case H.deserialize con of
    Left e -> print e
    Right i -> return ()

tmain :: IO ()
tmain =
  defaultMain
    [ bench "fr" $ nfIO fr,
      bench "fh" $ nfIO fh
    ]
