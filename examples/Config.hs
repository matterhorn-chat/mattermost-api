module Config where

import Data.Text
import Network.Mattermost.Types ( UserText )

data Config
  = Config
  { configUsername :: Text
  , configHostname :: Text
  , configTeam     :: UserText
  , configPort     :: Int
  , configPassword :: Text
  , configPath     :: Text
  }
