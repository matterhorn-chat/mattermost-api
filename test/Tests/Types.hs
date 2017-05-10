module Tests.Types
  ( Config(..)
  , TestM
  , TestState(..)
  )
where

import Data.Text (Text)
import Control.Monad.State.Lazy
import Control.Concurrent.MVar
import qualified Control.Concurrent.STM.TChan as STM

import Network.Mattermost
import Network.Mattermost.Types
import Network.Mattermost.WebSocket.Types

data Config
  = Config { configUsername :: Text
           , configHostname :: Text
           , configTeam     :: Text
           , configPort     :: Int
           , configPassword :: Text
           , configEmail    :: Text
           }

type TestM a = StateT TestState IO a

data TestState =
    TestState { tsPrinter        :: String -> IO ()
              , tsConfig         :: Config
              , tsConnectionData :: ConnectionData
              , tsSession        :: Maybe Session
              , tsDebug          :: Bool
              , tsWebsocketChan  :: STM.TChan WebsocketEvent
              , tsDone           :: MVar ()
              }
