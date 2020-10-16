{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Network.Mattermost.WebSocket
( MMWebSocket
, MMWebSocketTimeoutException
, mmWithWebSocket
, mmCloseWebSocket
, mmSendWSAction
, mmGetConnectionHealth
, module Network.Mattermost.WebSocket.Types
) where

import           Control.Concurrent (ThreadId, forkIO, myThreadId, threadDelay)
import qualified Control.Concurrent.STM.TQueue as Queue
import           Control.Exception (Exception, SomeException, catch, throwIO, throwTo, try, evaluate)
import           Control.Monad (forever)
import           Control.Monad.STM (atomically)
import           Data.Aeson (toJSON)
import           Data.Text.Encoding.Base64
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Lazy (toStrict)
import           Data.IORef
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import           Data.Typeable ( Typeable )
import           Network.Connection ( Connection
                                    , connectionClose
                                    , connectionGet
                                    , connectionPut
                                    )
import qualified Network.WebSockets as WS
import           Network.WebSockets.Stream (Stream, makeStream)

import           Network.Mattermost.Util
import           Network.Mattermost.Types.Base
import           Network.Mattermost.Types.Internal
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
  pingChan <- Queue.newTQueueIO
  pongChan <- Queue.newTQueueIO
  let pingAction = do
        now <- getCurrentTime
        doLog WebSocketPing
        atomically $ Queue.writeTQueue pingChan (P now)
  let pongAction = do
        now <- getCurrentTime
        doLog WebSocketPong
        atomically $ Queue.writeTQueue pongChan (P now)
  watchdogPId <- forkIO $ do
      let go = do
            P old <- atomically $ Queue.readTQueue pingChan
            threadDelay (n * 1000 * 1000)
            b <- atomically $ Queue.isEmptyTQueue pongChan
            if b
              then throwTo pId MMWebSocketTimeoutException
              else do
                P new <- atomically $ Queue.readTQueue pongChan
                atomicWriteIORef health (new `diffUTCTime` old)
                go
      go

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
          onPingAction
          WS.sendPing conn (B.pack (show n))
          loop (n+1)

mmWithWebSocket :: Session
                -> (Either String (Either WebsocketActionResponse WebsocketEvent) -> IO ())
                -> (MMWebSocket -> IO ())
                -> IO ()
mmWithWebSocket (Session cd tk) recv body = do
  con <- mkConnection (cdConnectionCtx cd) (cdHostname cd) (cdPort cd) (cdConnectionType cd)
  stream <- connectionToStream con
  health <- newIORef 0
  myId <- myThreadId
  let doLog = runLogger cd "websocket"
  (onPing, onPong, _) <- createPingPongTimeouts myId health 8 doLog
  let action c = do
        pId <- forkIO (pingThread onPing c `catch` cleanup)
        mId <- forkIO $ flip catch cleanup $ forever $ do
          result :: Either SomeException WS.DataMessage
                 <- try $ do
              msg <- WS.receiveDataMessage c
              msg `seq` return msg

          val <- case result of
                Left e -> do
                    doLog $ WebSocketResponse $ Right $ toJSON $
                        "Got exception on receiveDataMessage: " <> show e
                    throwIO e
                Right dataMsg -> do
                    -- The message could be either a websocket event or
                    -- an action response. Those have different Haskell
                    -- types, so we need to attempt to parse each.
                    evResult <- try $ evaluate $ WS.fromDataMessage dataMsg
                    case evResult of
                        Right wev -> return $ Right $ Right wev
                        Left (e1::SomeException) -> do
                            respResult <- try $ evaluate $ WS.fromDataMessage dataMsg
                            case respResult of
                                Right actionResp -> return $ Right $ Left actionResp
                                Left (e2::SomeException) -> do
                                    doLog $ WebSocketResponse $ Left $
                                        "Failed to parse (exceptions following): " <> show dataMsg
                                    doLog $ WebSocketResponse $ Left $
                                        "Failed to parse as a websocket event: " <> show e1
                                    doLog $ WebSocketResponse $ Left $
                                        "Failed to parse as a websocket action response: " <> show e2
                                    -- Log both exceptions, but throw
                                    -- the second. This isn't great
                                    -- because we don't know which
                                    -- exception is the *right* one. The
                                    -- best we can do is throw one of
                                    -- them and log both.
                                    throwIO e2

          doLog (WebSocketResponse $ case val of
                Left s -> Left s
                Right (Left v) -> Right $ toJSON v
                Right (Right v) -> Right $ toJSON v
                )
          recv val
        body (MMWS c health) `catch` propagate [mId, pId]
  path <- buildPath cd "/websocket"
  WS.runClientWithStream stream
                      (T.unpack $ cdHostname cd)
                      (T.unpack path)
                      WS.defaultConnectionOptions { WS.connectionOnPong = onPong }
                      (authHeaders cd tk)
                      action
  where cleanup :: SomeException -> IO ()
        cleanup _ = return ()
        propagate :: [ThreadId] -> SomeException -> IO ()
        propagate ts e = do
          sequence_ [ throwTo t e | t <- ts ]
          throwIO e

authHeaders :: ConnectionData -> Token -> WS.Headers
authHeaders (ConnectionData {cdBasicAuth = (Just ba)}) (Token tk) =
  [ ("Cookie", B.pack $ "MMAUTHTOKEN=" ++ tk ++ ";")
  , ("Authorization", B.pack $ "Basic " ++ (T.unpack $
                                             encodeBase64 $ T.intercalate
                                                       (T.pack ":") [ baUsername ba
                                                                    , baPassword ba
                                                                    ]
                                           )
    )
  ]

authHeaders _ (Token tk) =
  [("Authorization", "Bearer " <> B.pack tk)]


mmSendWSAction :: ConnectionData -> MMWebSocket -> WebsocketAction -> IO ()
mmSendWSAction cd (MMWS ws _) a = do
  runLogger cd "websocket" $ WebSocketRequest $ toJSON a
  WS.sendTextData ws a
