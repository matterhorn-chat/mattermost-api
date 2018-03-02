{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Mattermost.Util
( assertE
, noteE
, hoistE
, (~=)
, withConnection
, mkConnection
, connectionGetExact
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
                                    , TLSSettings(..)
                                    , connectionGet
                                    , connectTo )

import           Network.Mattermost.Types.Base
import           Network.Mattermost.Types.Internal

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
mkConnection :: ConnectionContext -> Hostname -> Port -> Bool -> IO MMConn
mkConnection connectionCtx hostname port useTLS = do
  newMMConn =<< (connectTo connectionCtx $ ConnectionParams
    { connectionHostname  = T.unpack hostname
    , connectionPort      = fromIntegral port
    , connectionUseSecure = if useTLS
                               then Just (TLSSettingsSimple False False False)
                               else Nothing
    , connectionUseSocks  = Nothing
    })

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
