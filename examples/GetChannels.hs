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

import           Network.Mattermost

import           Config
import           LocalConfig -- You will need to define a function:
                             -- getConfig :: IO Config
                             -- See Config.hs

main :: IO ()
main = do
  config <- getConfig -- see LocalConfig import
  ctx    <- initConnectionContext
  let cd = mkConnectionData (configHostname config)
                            (fromIntegral (configPort config)) ctx

  let login = Login { username = configUsername config
                    , password = configPassword config
                    }

  (session, mmUser) <- join (hoistE <$> mmLogin cd login)
  putStrLn "Authenticated as:"
  pPrint mmUser

  i <- mmGetInitialLoad session
  forM_ (initialLoadTeams i) $ \t -> do
    when (teamName t == configTeam config) $ do
      chans <- mmGetChannels session (teamId t)
      forM_ chans $ \chan -> do
        channel <- mmGetChannel session (teamId t) (channelId chan)
        pPrint channel
        putStrLn ""
