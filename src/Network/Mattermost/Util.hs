{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Mattermost.Util
( assertE
, noteE
, hoistE
, (~=)
, dropTrailingChar
, withConnection
, connectionGetExact
) where

import           Data.Default ( def )
import           Data.Char ( toUpper )
import qualified Data.ByteString.Char8 as B

import           Control.Exception ( Exception
                                   , throwIO
                                   , bracket )
import           Network.Connection ( Connection
                                    , ConnectionParams(..)
                                    , connectionGet
                                    , connectionClose
                                    , connectTo )

import           Network.Mattermost.Types

noteE :: Exception e => Maybe r -> e -> IO r
noteE Nothing  e  = throwIO e
noteE (Just r) _  = pure    r

hoistE :: Exception e => Either e r -> IO r
hoistE (Left e)  = throwIO e
hoistE (Right r) = pure    r

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
withConnection :: ConnectionData -> (Connection -> IO a) -> IO a
withConnection cd action =
  bracket (connectTo (cdConnectionCtx cd) $ ConnectionParams
            { connectionHostname  = cdHostname cd
            , connectionPort      = fromIntegral (cdPort cd)
            , connectionUseSecure = Just def
            , connectionUseSocks  = Nothing
            }
          )
          connectionClose
          action

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
