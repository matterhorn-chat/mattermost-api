{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mattermost.WebSocket where

import           Control.Monad (forever)
import           Data.Aeson (Value, eitherDecode)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Lazy (ByteString, toStrict)
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
import           Text.Show.Pretty ( pPrint )


connectionToStream :: Connection -> IO Stream
connectionToStream con = makeStream rd wr
  where wr Nothing   = connectionClose con
        wr (Just bs) = connectionPut con (toStrict bs)
        rd = do
          bs <- connectionGet con 1024
          return $ if B.null bs
            then Nothing
            else (Just bs)

mmWsConnect :: ConnectionData -> Token -> IO ()
mmWsConnect cd (Token tk) = do
  con <- mkConnection cd
  stream <- connectionToStream con
  WS.runClientWithStream stream
                      (cdHostname cd)
                      "/api/v3/users/websocket"
                      WS.defaultConnectionOptions
                      [ ("Authorization", "Bearer " <> B.pack tk) ]
                      printStuff

printStuff :: WS.Connection -> IO ()
printStuff con = forever $ do
  msg <- WS.receiveData con
  pPrint (eitherDecode msg :: Either String WebsocketEvent)
