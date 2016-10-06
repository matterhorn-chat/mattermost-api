{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Network.Mattermost.WebSocket
( MMWebSocket
, MMWebSocketTimeoutException
, mmWithWebSocket
, mmCloseWebSocket
, mmGetConnectionHealth
) where

import           Control.Concurrent (ThreadId, forkIO, myThreadId, threadDelay)
import qualified Control.Concurrent.STM.TChan as Chan
import           Control.Exception (Exception, SomeException, catch, throwIO, throwTo)
import           Control.Monad (forever)
import           Control.Monad.STM (atomically)
import           Data.Aeson (toJSON)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Lazy (toStrict)
import           Data.IORef
import           Data.Monoid ((<>))
import           Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import           Data.Typeable ( Typeable )
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
  deriving (Show, Typeable)

instance Exception MMWebSocketTimeoutException where

data PEvent = P UTCTime

createPingPongTimeouts :: ThreadId
                       -> IORef NominalDiffTime
                       -> Int
                       -> (LogEventType -> IO ())
                       -> IO (IO (), IO (), ThreadId)
createPingPongTimeouts pId health n doLog = do
  pingChan <- Chan.newTChanIO
  pongChan <- Chan.newTChanIO
  let pingAction = do
        now <- getCurrentTime
        doLog WebSocketPing
        atomically $ Chan.writeTChan pingChan (P now)
  let pongAction = do
        now <- getCurrentTime
        doLog WebSocketPong
        atomically $ Chan.writeTChan pongChan (P now)
  watchdogPId <- forkIO $ forever $ do
    P old <- atomically $ Chan.readTChan pingChan
    threadDelay (n * 1000 * 1000)
    b <- atomically $ Chan.isEmptyTChan pongChan
    if b
      then throwTo pId MMWebSocketTimeoutException
      else do
        P new <- atomically $ Chan.readTChan pingChan
        atomicWriteIORef health (new `diffUTCTime` old)

  return (pingAction, pongAction, watchdogPId)

mmCloseWebSocket :: MMWebSocket -> IO ()
mmCloseWebSocket (MMWS c _) = WS.sendClose c B.empty

mmGetConnectionHealth :: MMWebSocket -> IO NominalDiffTime
mmGetConnectionHealth (MMWS _ h) = readIORef h

pingThread :: IO () -> WS.Connection -> IO ()
pingThread onPingAction conn = loop 0
  where loop :: Int -> IO ()
        loop n = do
          threadDelay (10 * 1000 * 1000)
          WS.sendPing conn (B.pack (show n))
          onPingAction
          loop (n+1)

mmWithWebSocket :: ConnectionData
                -> Token
                -> (WebsocketEvent -> IO ())
                -> (MMWebSocket -> IO ())
                -> IO ()
mmWithWebSocket cd (Token tk) recv body = do
  con <- mkConnection cd
  stream <- connectionToStream con
  health <- newIORef 0
  myId <- myThreadId
  let doLog = runLogger cd "websocket"
  (onPing, onPong, wId) <- createPingPongTimeouts myId health 8 doLog
  let action c = do
        pId <- forkIO (pingThread onPing c `catch` cleanup)
        mId <- forkIO $ flip catch cleanup $ forever $ do
          p <- WS.receiveData c
          doLog (WebSocketResponse (toJSON p))
          recv p
        body (MMWS c health) `catch` propagate [mId, pId, wId]
  WS.runClientWithStream stream
                      (cdHostname cd)
                      "/api/v3/users/websocket"
                      WS.defaultConnectionOptions { WS.connectionOnPong = onPong }
                      [ ("Authorization", "Bearer " <> B.pack tk) ]
                      (\ c -> action c)
  where cleanup :: SomeException -> IO ()
        cleanup _ = return ()
        propagate :: [ThreadId] -> SomeException -> IO ()
        propagate ts e = do
          sequence_ [ throwTo t e | t <- ts ]
          throwIO e
