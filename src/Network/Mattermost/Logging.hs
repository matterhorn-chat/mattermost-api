module Network.Mattermost.Logging
( -- * Logging-Related Types
  Logger
, LogEvent(..)
, LogEventType(..)
  -- * Basic Loggers
, mmLoggerInfo
, mmLoggerInfoFilter
, mmLoggerDebug
, mmLoggerDebugFilter
  -- ** @stderr@ variants
, mmLoggerInfoErr
, mmLoggerInfoFilterErr
, mmLoggerDebugErr
, mmLoggerDebugFilterErr
) where

import Data.Time.Clock (getCurrentTime)
import System.IO (Handle, hFlush, hPutStr, stderr)

import Network.Mattermost.BaseTypes

-- | 'mmLoggerDebugFilter' is the same as 'mmLoggerDebug' but takes
--   a user-defined predicate that it uses to select which events to
--   log before writing them to the provided 'Handle'
mmLoggerDebugFilter :: (LogEvent -> Bool) -> Handle -> Logger
mmLoggerDebugFilter p h l
  | p l       = mmLoggerDebug h l
  | otherwise = return ()

-- | 'mmLoggerDebug' prints the full data of every logging event to
--   the provided 'Handle'.
mmLoggerDebug :: Handle -> Logger
mmLoggerDebug h LogEvent { logFunction = f, logEventType = e } = do
  now <- getCurrentTime
  mapM_ (hPutStr h)
    [ "[", show now, "] ", f, ": ", show e, "\n" ]
  hFlush h

-- | 'mmLoggerDebugErr' prints the full data of every logging event
--   to 'stderr'.
mmLoggerDebugErr :: Logger
mmLoggerDebugErr = mmLoggerDebug stderr

-- | 'mmLoggerDebugFilterErr' takes a user-defined predicate that
--   it uses to select which events to log before logging them to
--   'stderr'.
mmLoggerDebugFilterErr :: (LogEvent -> Bool) -> Logger
mmLoggerDebugFilterErr p l = mmLoggerDebugFilter p stderr l


-- | 'mmLoggerInfoFilter' is the same as 'mmLoggerInfo' but takes
--   a user-defined predicate that it uses to select which events to
--   log before writing them to the provided 'Handle'
mmLoggerInfoFilter :: (LogEvent -> Bool) -> Handle -> Logger
mmLoggerInfoFilter p h l
  | p l       = mmLoggerInfo h l
  | otherwise = return ()

-- | 'mmLoggerInfo' prints which calls are happening and which
--   endpoints are being hit, but without the payloads.
mmLoggerInfo :: Handle -> Logger
mmLoggerInfo h LogEvent { logFunction = f, logEventType = e } = do
  now <- getCurrentTime
  mapM_ (hPutStr h)
    [ "[", show now, "] ", f, ": ", info e, "\n" ]
  hFlush h
  where info (HttpRequest m s _) = show m ++ " " ++ s
        info (HttpResponse n s _) = show n ++ " from " ++ s
        info (WebSocketRequest _) = "websocket request"
        info (WebSocketResponse _) = "websocket request"
        info WebSocketPing = "websocket ping"
        info WebSocketPong = "websocket pong"

-- | 'mmLoggerInfoErr' prints request/response data without payloads
--   to 'stderr'
mmLoggerInfoErr :: Logger
mmLoggerInfoErr = mmLoggerInfo stderr

-- | 'mmLoggerInfoFilterErr' takes a user-defined predicate that
--   it uses to select which events to log before logging them to
--   'stderr'.
mmLoggerInfoFilterErr :: (LogEvent -> Bool) -> Logger
mmLoggerInfoFilterErr p l = mmLoggerInfoFilter p stderr l
