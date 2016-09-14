{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mattermost.WebSocket
( MMWebSocket
, MMWebSocketTimeoutException
, mmWithWebSocket
, mmCloseWebSocket
, mmGetConnectionHealth
) where

import           Control.Concurrent (forkIO, threadDelay)
import qualified Control.Concurrent.STM.TChan as Chan
import           Control.Exception (Exception, catch, throwIO)
import           Control.Monad (forever, when)
import           Control.Monad.STM (atomically)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Lazy (toStrict)
import           Data.IORef
import           Data.Monoid ((<>))
import           Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import           Network.Connection ( Connection
                                    , connectionClose
                                    , connectionGet
                                    , connectionPut
                                    )
import qualified Network.WebSockets as WS
import           Network.WebSockets.Stream (Stream, makeStream)

import           Network.Mattermost
import           Network.Mattermost.Util
import           Network.Mattermost.Types
import           Network.Mattermost.WebSocket.Types


connectionToStream :: Connection -> IO Stream
connectionToStream con = makeStream rd wr
  where wr Nothing   = connectionClose con
        wr (Just bs) = connectionPut con (toStrict bs)
        rd = do
          bs <- connectionGet con 1024
          return $ if B.null bs
            then Nothing
            else Just bs

data MMWebSocket = MMWS WS.Connection (IORef NominalDiffTime)

data MMWebSocketTimeoutException = MMWebSocketTimeoutException
  deriving (Show)

instance Exception MMWebSocketTimeoutException where

data PEvent = P UTCTime

createPingPongTimeouts :: IORef NominalDiffTime -> Int -> IO (IO (), IO ())
createPingPongTimeouts health n = do
  pingChan <- Chan.newTChanIO
  pongChan <- Chan.newTChanIO
  let pingAction = do
        now <- getCurrentTime
        atomically $ Chan.writeTChan pingChan (P now)
  let pongAction = do
        now <- getCurrentTime
        atomically $ Chan.writeTChan pongChan (P now)
  _  <- forkIO $ forever $ do
    P old <- atomically $ Chan.readTChan pingChan
    threadDelay (n * 1000 * 1000)
    b <- atomically $ Chan.isEmptyTChan pongChan
    when b $ throwIO MMWebSocketTimeoutException
    P new <- atomically $ Chan.readTChan pingChan
    atomicWriteIORef health (new `diffUTCTime` old)
    -- something with connection health?
    return ()

  return (pingAction, pongAction)

mmCloseWebSocket :: MMWebSocket -> IO ()
mmCloseWebSocket (MMWS c _) = WS.sendClose c B.empty

mmGetConnectionHealth :: MMWebSocket -> IO NominalDiffTime
mmGetConnectionHealth (MMWS _ h) = readIORef h

mmWithWebSocket :: ConnectionData
                -> Token
                -> (WebsocketEvent -> IO ())
                -> (MMWebSocket -> IO ())
                -> IO ()
mmWithWebSocket cd (Token tk) recv body = do
  con <- mkConnection cd
  stream <- connectionToStream con
  health <- newIORef 0
  (onPing, onPong) <- createPingPongTimeouts health 8
  WS.runClientWithStream stream
                      (cdHostname cd)
                      "/api/v3/users/websocket"
                      WS.defaultConnectionOptions { WS.connectionOnPong = onPong }
                      [ ("Authorization", "Bearer " <> B.pack tk) ]
                      (\ c -> action health c onPing `catch` cleanup)
  where action health c onPing = do
          _ <- forkIO (go onPing c 1)
          _ <- forkIO $ forever (WS.receiveData c >>= recv)
          body (MMWS c health)
        go :: IO () -> WS.Connection -> Int -> IO ()
        go onPing c n = do
          threadDelay (10 * 1000 * 1000)
          WS.sendPing c (B.pack (show n))
          onPing
          go onPing c (n + 1)
        cleanup :: WS.ConnectionException -> IO ()
        cleanup _ = return ()
