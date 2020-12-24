{-# LANGUAGE OverloadedStrings #-}

module Network.HESP.TCP (sendMsg, recvMsg, sendMsgs) where

import Network.HESP.Protocol (encode, mParser)
import Network.HESP.Types (Message)
import Z.Data.Builder (build)
import Z.Data.Parser (ParseError)
import Z.IO
  ( BufferedInput,
    BufferedOutput,
    flushBuffer,
    readParser,
    writeBuffer,
  )

sendMsg :: BufferedOutput -> Message -> IO ()
sendMsg o m = writeBuffer o (build $ encode m) >> flushBuffer o

recvMsg :: BufferedInput -> IO (Either ParseError Message)
recvMsg = readParser mParser

sendMsgs :: BufferedOutput -> [Message] -> IO ()
sendMsgs o ms = mapM_ (\m -> writeBuffer o (build $ encode m)) ms >> flushBuffer o

-- server :: IO ()
-- server = startTCPServer defaultTCPServerConfig $ \tcp -> do
--   i <- newBufferedInput tcp
--   o <- newBufferedOutput tcp
--   forever $ readBuffer i >>= writeBuffer o >> flushBuffer o
--
-- client :: IO ()
-- client = withResource (initTCPClient defaultTCPClientConfig) $ \tcp -> do
--   i <- newBufferedInput tcp
--   o <- newBufferedOutput tcp
--   forever $ do
--     v2 <- F.quickReadFile "res"
--     t1 <- getCurrentTime
--     print $ show (V.length v2 `div` 1024) ++ "k"
--     writeBuffer o v2
--     flushBuffer o
--
--     readParser mParser i >>= \x -> do
--       case x of
--         Left e -> error $ show e
--         Right v -> do
--           t2 <- getCurrentTime
--           print $ diffUTCTime t2 t1
--           return ()
