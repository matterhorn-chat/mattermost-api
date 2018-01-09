{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Mattermost.Util
( assertE
, noteE
, hoistE
, (~=)
, dropTrailingChar
, withConnection
, mkConnection
, connectionGetExact
) where

import           Data.Char ( toUpper )
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

import           Control.Exception ( Exception
                                   , throwIO
                                   , bracket )
import           Network.Connection ( Connection
                                    , ConnectionParams(..)
                                    , TLSSettings(..)
                                    , connectionGet
                                    , connectionGetLine
                                    , connectionPut
                                    , connectionClose
                                    , connectTo )
import qualified Network.HTTP.Stream as HTTP

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

-- | HTTP ends newlines with \r\n sequence, but the 'connection' package doesn't
-- know this so we need to drop the \r after reading lines. This should only be
-- needed in your compatibility with the HTTP library.
dropTrailingChar :: B.ByteString -> B.ByteString
dropTrailingChar bs | not (B.null bs) = B.init bs
dropTrailingChar _ = ""

-- | Creates a new connection to 'Hostname' from an already initialized 'ConnectionContext'.
-- Internally it uses 'bracket' to cleanup the connection.
withConnection :: ConnectionData -> (MMConn -> IO a) -> IO a
withConnection cd action =
  bracket (MMConn <$> mkConnection cd)
          (connectionClose . fromMMConn)
          action

maxLineLength :: Int
maxLineLength = 2^(16::Int)

newtype MMConn = MMConn { fromMMConn :: Connection }

-- | This instance allows us to use 'simpleHTTP' from 'Network.HTTP.Stream' with
-- connections from the 'connection' package.
instance HTTP.Stream MMConn where
  readLine   con       = Right . B.unpack . dropTrailingChar <$> connectionGetLine maxLineLength (fromMMConn con)
  readBlock  con n     = Right . B.unpack <$> connectionGetExact (fromMMConn con) n
  writeBlock con block = Right <$> connectionPut (fromMMConn con) (B.pack block)
  close      con       = connectionClose (fromMMConn con)
  closeOnEnd _   _     = return ()


-- | Creates a connection from a 'ConnectionData' value, returning it. It
--   is the user's responsibility to close this appropriately.
mkConnection :: ConnectionData -> IO Connection
mkConnection cd = do
  connectTo (cdConnectionCtx cd) $ ConnectionParams
    { connectionHostname  = T.unpack $ cdHostname cd
    , connectionPort      = fromIntegral (cdPort cd)
    , connectionUseSecure = if cdUseTLS cd
                               then Just (TLSSettingsSimple False False False)
                               else Nothing
    , connectionUseSocks  = Nothing
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
