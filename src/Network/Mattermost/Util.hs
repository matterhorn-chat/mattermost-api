{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Mattermost.Util
( ConnectionType(..)
, assertE
, noteE
, hoistE
, (~=)
, withConnection
, mkConnection
, connectionGetExact
, buildPath
) where

import           Control.Exception (finally, onException)
import           Data.Char ( toUpper )
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

import           Control.Exception ( Exception
                                   , throwIO )
import           Data.Pool (takeResource, putResource, destroyResource)
import           Network.Connection ( Connection
                                    , ConnectionContext
                                    , ConnectionParams(..)
                                    , ProxySettings(..)
                                    , TLSSettings(..)
                                    , connectionGet
                                    , connectTo )

import           Network.Mattermost.Types.Base
import           Network.Mattermost.Types.Internal
import           Network.Mattermost.Proxy

-- | This unwraps a 'Maybe' value, throwing a provided exception
--   if the value is 'Nothing'.
noteE :: Exception e => Maybe r -> e -> IO r
noteE Nothing  e  = throwIO e
noteE (Just r) _  = pure    r

-- | This unwraps an 'Either' value, throwing the contained exception
--   if the 'Either' was a 'Left' value.
hoistE :: Exception e => Either e r -> IO r
hoistE (Left e)  = throwIO e
hoistE (Right r) = pure    r

-- | This asserts that the provided 'Bool' is 'True', throwing a
--   provided exception is the argument was 'False'.
assertE :: Exception e => Bool -> e -> IO ()
assertE True  _ = pure    ()
assertE False e = throwIO e

-- | Case Insensitive string comparison
(~=) :: String -> String -> Bool
a ~= b = map toUpper a == map toUpper b

-- | Creates a new connection to 'Hostname' from an already initialized
-- 'ConnectionContext'.
withConnection :: ConnectionData -> (MMConn -> IO a) -> IO a
withConnection cd action = do
    (conn, lp) <- takeResource (cdConnectionPool cd)
    (action conn `onException` closeMMConn conn) `finally` do
        c <- isConnected conn
        if c then
             putResource lp conn else
             destroyResource (cdConnectionPool cd) lp conn

-- | Creates a connection from a 'ConnectionData' value, returning it. It
--   is the user's responsibility to close this appropriately.
--
-- This function respects ALL_PROXY, HTTP_PROXY, HTTPS_PROXY, and
-- NO_PROXY environment variables for controlling whether the resulting
-- connection uses a proxy. However, note:
--
-- * Only SOCKS version 4 and 5 proxies are supported using socks4://
--   and socks5:// URIs, and
-- * No proxy authentication is supported.
mkConnection :: ConnectionContext -> Hostname -> Port -> ConnectionType -> IO Connection
mkConnection ctx host port connTy = do
  proxy' <- case connTy of
     ConnectHTTPS _ -> proxyForScheme HTTPS
     ConnectHTTP -> return Nothing

  canUseProxy <- proxyHostPermitted (T.unpack host)
  let proxy = if canUseProxy then proxy' else Nothing
  connectTo ctx $ ConnectionParams
    { connectionHostname  = T.unpack host
    , connectionPort      = fromIntegral port
    , connectionUseSecure = case connTy of
        ConnectHTTP -> Nothing
        ConnectHTTPS requireTrustedCert ->
            -- The first argument to TLSSettingsSimple is whether to
            -- *disable* cert validation. If requireTrustedCert is True,
            -- we want that argument to be False to force validation.
            Just (TLSSettingsSimple (not requireTrustedCert) False False)
    , connectionUseSocks  = do
        (ty, cHost, cPort) <- proxy
        case ty of
            Socks -> return $ SockSettingsSimple cHost (toEnum cPort)
    }

-- | Get exact count of bytes from a connection.
--
-- The size argument is the exact amount that must be returned to the user.
-- The call will wait until all data is available.  Hence, it behaves like
-- 'B.hGet'.
--
-- On end of input, 'connectionGetExact' will throw an 'E.isEOFError'
-- exception.
-- Taken from: https://github.com/vincenthz/hs-connection/issues/9
connectionGetExact :: Connection -> Int -> IO B.ByteString
connectionGetExact con n = loop B.empty 0
  where loop bs y
          | y == n = return bs
          | otherwise = do
            next <- connectionGet con (n - y)
            loop (B.append bs next) (y + (B.length next))

-- | Build a full URL path from the path of an API endpoint
buildPath :: ConnectionData -> String -> String
buildPath cd endpoint = cdUrlPath cd ++ "/api/v4" ++ endpoint
