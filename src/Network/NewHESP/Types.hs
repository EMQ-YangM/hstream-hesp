{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.NewHESP.Types where

import Control.Monad (forM, replicateM)
import Criterion.Main
import qualified Data.ByteString as B
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
import           Criterion.Main

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
              (10, Array <$> clist),
              (0, Boolean <$> arbitrary),
              (0, Push <$> arbitrary <*> clist),
              (0, Map <$> cmap)
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
  firstWord <- P.peek
  case w2c firstWord of
    '+' -> do
      P.skipWord8
      v <- P.takeWhile ((/= '\r') . w2c)
      eol
      return (SimpleString v)
    '$' -> do
      P.skipWord8
      len <- P.int :: P.Parser Int
      eol
      v <- P.take len
      eol
      return (BulkString v)
    '-' -> do
      P.skipWord8
      et <- P.takeWhile ((/= ' ') . w2c)
      P.skipWord8
      em <- P.takeWhile ((/= '\r') . w2c)
      eol
      return (SimpleError et em)
    '#' -> do
      P.skipWord8
      v <- P.peek
      P.skipWord8
      eol
      case w2c v of
        'f' -> return $ Boolean False
        't' -> return $ Boolean True
        _ -> error "boolean value error"
    ':' -> do
      P.skipWord8
      v <- P.integer
      eol
      return $ Integer v
    '*' -> do
      P.skipWord8
      len <- P.int :: P.Parser Int
      eol
      v <- replicateM len decode
      return (Array (V.pack v))
    '>' -> do
      P.skipWord8
      len <- P.int :: P.Parser Int
      eol
      (BulkString h) : t <- replicateM len decode
      return $ Push h (V.pack t)
    '%' -> do
      P.skipWord8
      len <- P.int :: P.Parser Int
      eol
      vs <- replicateM (2 * len) decode
      let temp = group2 vs
      return $ Map $ M.pack temp
    _ -> error "strange happened"

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

-- let v1 = build $ (encode v <> encode va)
-- case P.parse decode v1 of
--   (_, Left e) -> print e
--   (l, Right v) -> do
--     print v
--     print $ P.parse decode l