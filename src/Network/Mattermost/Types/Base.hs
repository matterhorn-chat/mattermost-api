-- | These are the fundamental types that are used for building
-- everything else.  Specifically, these types are used by the
-- Network.Mattermost.Types.Internal, but are not subject to the
-- cautions that as associated with the latter.

module Network.Mattermost.Types.Base where

import qualified Data.Aeson as A
import           Data.Text (Text)
import           Data.Time.Clock ( UTCTime )
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
  = HttpRequest RequestMethod String (Maybe String)
  | HttpResponse Int String (Maybe String)
  | WebSocketRequest A.Value
  | WebSocketResponse (Either String String)
  -- ^ Left means we got an exception trying to parse the response;
  -- Right means we succeeded and here it is.
  | WebSocketPing
  | WebSocketPong
    deriving (Eq, Show)


-- | The time as provided by the Server.  This is a wrapper designed
-- to warn against naive comparisons to local times: the server's time
-- and local times are not necessarily synchronized.

newtype ServerTime = ServerTime { withServerTime :: UTCTime }
    deriving (Eq, Ord, Read, Show)
