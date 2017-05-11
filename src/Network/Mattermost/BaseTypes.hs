module Network.Mattermost.BaseTypes where

import qualified Data.Aeson as A
import           Data.Text (Text)
import           Network.HTTP.Base (RequestMethod)

type Hostname = Text
type Port     = Int

-- | A 'Logger' is any function which responds to log events:
type Logger = LogEvent -> IO ()

-- | If there is a 'Logger' in the 'ConnectionData' struct, it will
--   be sporadically called with values of type 'LogEvent'.
data LogEvent = LogEvent
  { logFunction  :: String
  , logEventType :: LogEventType
  } deriving (Eq, Show)

-- | A 'LogEventType' describes the particular event that happened
data LogEventType
  = HttpRequest RequestMethod String (Maybe A.Value)
  | HttpResponse Int String (Maybe A.Value)
  | WebSocketRequest A.Value
  | WebSocketResponse A.Value
  | WebSocketPing
  | WebSocketPong
    deriving (Eq, Show)
