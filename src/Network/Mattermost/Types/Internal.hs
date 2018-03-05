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
import Data.Pool (Pool)
import qualified Network.Connection as C
import Control.Exception (finally)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Network.HTTP.Headers (Header, HeaderName(..), mkHeader)
import qualified Network.HTTP.Stream as HTTP
import qualified Data.ByteString.Char8 as B
import Network.Mattermost.Types.Base

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

data ConnectionData
  = ConnectionData
  { cdHostname       :: Hostname
  , cdPort           :: Port
  , cdAutoClose      :: AutoClose
  , cdConnectionPool :: Pool MMConn
  , cdConnectionCtx  :: C.ConnectionContext
  , cdToken          :: Maybe Token
  , cdLogger         :: Maybe Logger
  , cdUseTLS         :: Bool
  }
