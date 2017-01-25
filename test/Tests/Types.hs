module Tests.Types
  ( Config(..)
  )
where

import Data.Text (Text)

data Config
  = Config { configUsername :: Text
           , configHostname :: Text
           , configTeam     :: Text
           , configPort     :: Int
           , configPassword :: Text
           , configEmail    :: Text
           }
