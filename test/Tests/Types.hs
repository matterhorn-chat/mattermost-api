module Tests.Types
  ( TestConfig(..)
  , TestM
  , TestState(..)
  )
where

import Data.Text (Text)
import Control.Monad.State.Lazy
import Control.Concurrent.MVar
import qualified Control.Concurrent.STM.TChan as STM

import Network.Mattermost.Types.Internal
import Network.Mattermost.Types (Session)
import Network.Mattermost.WebSocket.Types

data TestConfig
  = TestConfig
        { configUsername :: Text
        , configHostname :: Text
        , configTeam     :: Text
        , configPort     :: Int
        , configPassword :: Text
        , configEmail    :: Text
        }

type TestM a = StateT TestState IO a

data TestState =
    TestState { tsPrinter        :: String -> IO ()
              , tsConfig         :: TestConfig
              , tsConnectionData :: ConnectionData
              , tsSession        :: Maybe Session
              , tsDebug          :: Bool
              , tsWebsocketChan  :: STM.TChan WebsocketEvent
              , tsDone           :: MVar ()
              }
