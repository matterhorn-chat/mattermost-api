module Tests.Types
  ( Config(..)
  , TestM
  , TestState(..)
  )
where

import Data.Text (Text)
import Control.Monad.State.Lazy

import Network.Mattermost

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
              , tsToken          :: Maybe Token
              , tsDebug          :: Bool
              }
