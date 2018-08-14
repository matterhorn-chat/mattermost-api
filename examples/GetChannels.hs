{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- Note: See LocalConfig.hs to configure example for your server
module Main (main) where
import           Text.Show.Pretty ( pPrint )
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Network.Connection
import           Data.Foldable
import           Control.Monad ( when, join )

import           Network.Mattermost.Endpoints
import           Network.Mattermost.Types
import           Network.Mattermost.Util

import           Config
import           LocalConfig -- You will need to define a function:
                             -- getConfig :: IO Config
                             -- See Config.hs

main :: IO ()
main = do
  config <- getConfig -- see LocalConfig import
  cd <- initConnectionData (configHostname config)
                           (fromIntegral (configPort config)) defaultConnectionPoolConfig

  let login = Login { username = configUsername config
                    , password = configPassword config
                    }

  (session, mmUser) <- join (hoistE <$> mmLogin cd login)
  putStrLn "Authenticated as:"
  pPrint mmUser

  teams <- mmGetUsersTeams UserMe session
  forM_ teams $ \t -> do
    when (teamName t == configTeam config) $ do
      chans <- mmGetChannelsForUser UserMe (getId t) session
      forM_ chans $ \chan -> do
        pPrint chan
        putStrLn ""
