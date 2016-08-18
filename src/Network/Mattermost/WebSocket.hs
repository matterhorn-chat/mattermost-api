{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mattermost.WebSocket
( MMWebSocket
, mmWithWebSocket
, mmCloseWebSocket
) where

import           Control.Concurrent (forkIO)
import           Control.Exception (catch)
import           Control.Monad (forever)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Lazy (toStrict)
import           Data.Monoid ((<>))
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
            else (Just bs)

newtype MMWebSocket = MMWS WS.Connection

mmCloseWebSocket :: MMWebSocket -> IO ()
mmCloseWebSocket (MMWS c) = WS.sendClose c B.empty

mmWithWebSocket :: ConnectionData
                -> Token
                -> (WebsocketEvent -> IO ())
                -> (MMWebSocket -> IO ())
                -> IO ()
mmWithWebSocket cd (Token tk) recv body = do
  con <- mkConnection cd
  stream <- connectionToStream con
  WS.runClientWithStream stream
                      (cdHostname cd)
                      "/api/v3/users/websocket"
                      WS.defaultConnectionOptions
                      [ ("Authorization", "Bearer " <> B.pack tk) ]
                      (\ c -> action c `catch` cleanup)
  where action c = do
          _ <- forkIO $ forever (WS.receiveData c >>= recv)
          body (MMWS c)
        cleanup :: WS.ConnectionException -> IO ()
        cleanup _ = return ()
