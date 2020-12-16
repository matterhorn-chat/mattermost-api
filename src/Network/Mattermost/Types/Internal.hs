{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The types defined in this module are exported to facilitate
-- efforts such as QuickCheck and other instrospection efforts, but
-- users are advised to avoid using these types wherever possible:
-- they can be used in a manner that would cause significant
-- disruption and may be subject to change without being reflected in
-- the mattermost-api version.

module Network.Mattermost.Types.Internal where

import Control.Monad (when)
import Data.Monoid ((<>))
import Data.Pool (Pool)
import qualified Network.Connection as C
import Control.Exception (finally)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Network.HTTP.Headers (Header, HeaderName(..), mkHeader)
import qualified Network.HTTP.Base as HTTP
import qualified Network.HTTP.Stream as HTTP
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Char8 as B
import Network.Mattermost.Types.Base
import qualified Data.Text as T

data Token = Token String
  deriving (Read, Show, Eq, Ord)

getTokenString :: Token -> String
getTokenString (Token s) = s

data AutoClose = No | Yes
  deriving (Read, Show, Eq, Ord)

-- | We return a list of headers so that we can treat
-- the headers like a monoid.
autoCloseToHeader :: AutoClose -> [Header]
autoCloseToHeader No  = []
autoCloseToHeader Yes = [mkHeader HdrConnection "Close"]

data MMConn = MMConn { fromMMConn :: C.Connection
                     , connConnected :: IORef Bool
                     }

closeMMConn :: MMConn -> IO ()
closeMMConn c = do
    conn <- readIORef $ connConnected c
    when conn $
        C.connectionClose (fromMMConn c)
            `finally` (writeIORef (connConnected c) False)

newMMConn :: C.Connection -> IO MMConn
newMMConn c = do
    v <- newIORef True
    return $ MMConn c v

isConnected :: MMConn -> IO Bool
isConnected = readIORef . connConnected

maxLineLength :: Int
maxLineLength = 2^(16::Int)

-- | HTTP ends newlines with \r\n sequence, but the 'connection' package doesn't
-- know this so we need to drop the \r after reading lines. This should only be
-- needed in your compatibility with the HTTP library.
dropTrailingChar :: B.ByteString -> B.ByteString
dropTrailingChar bs | not (B.null bs) = B.init bs
dropTrailingChar _ = ""

-- | This instance allows us to use 'simpleHTTP' from 'Network.HTTP.Stream' with
-- connections from the 'connection' package.
instance HTTP.Stream MMConn where
  readLine   con       = Right . B.unpack . dropTrailingChar <$> C.connectionGetLine maxLineLength (fromMMConn con)
  readBlock  con n     = Right . B.unpack <$> C.connectionGetExact (fromMMConn con) n
  writeBlock con block = Right <$> C.connectionPut (fromMMConn con) (B.pack block)
  close      con       = C.connectionClose (fromMMConn con)
  closeOnEnd _   _     = return ()

data ConnectionType =
    ConnectHTTPS Bool
    -- ^ Boolean is whether to require trusted certificate
    | ConnectHTTP
    -- ^ Make an insecure connection over HTTP
    deriving (Eq, Show, Read)

data RequestTransformer
  = RequestTransformer
  { rtHttpTransformer :: ConnectionData -> HTTP.Request_String -> HTTP.Request_String
  , rtWsTransformer :: ConnectionData -> WS.Headers -> WS.Headers
  }

data ConnectionData
  = ConnectionData
  { cdHostname       :: Hostname
  , cdPort           :: Port
  , cdUrlPath        :: T.Text
  , cdAutoClose      :: AutoClose
  , cdConnectionPool :: Pool MMConn
  , cdConnectionCtx  :: C.ConnectionContext
  , cdToken          :: Maybe Token
  , cdReqTransformer :: RequestTransformer
  , cdLogger         :: Maybe Logger
  , cdConnectionType :: ConnectionType
  }


newtype ServerBaseURL = ServerBaseURL T.Text
                      deriving (Eq, Show)

connectionDataURL :: ConnectionData -> ServerBaseURL
connectionDataURL cd =
    let scheme = case cdConnectionType cd of
            ConnectHTTPS {} -> "https"
            ConnectHTTP {} -> "http"
        host = cdHostname cd
        port = T.pack $
               if cdConnectionType cd == ConnectHTTP
               then if cdPort cd == 80 then "" else ":" <> show (cdPort cd)
               else if cdPort cd == 443 then "" else ":" <> show (cdPort cd)
        path1 = cdUrlPath cd
        path2 = if "/" `T.isPrefixOf` path1
                then path1 else "/" <> path1
    in ServerBaseURL $ scheme <> "://" <> host <> port <> path2
