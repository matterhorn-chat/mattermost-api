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
  let cd = mkConnectionData (T.unpack (configHostname config))
                            (fromIntegral (configPort config)) ctx

  let login = Login { username = configUsername config
                    , password = configPassword config
                    , teamname = configTeam     config }

  (token, mmUser) <- join (hoistE <$> mmLogin cd login)
  putStrLn "Authenticated as:"
  pPrint mmUser

  teamMap <- mmGetTeams cd token
  forM_ (HM.elems teamMap) $ \t -> do
    when (teamName t == T.unpack (configTeam config)) $ do
      Channels chans _ <- mmGetChannels cd token (teamId t)
      forM_ chans $ \chan -> do
        channel <- mmGetChannel cd token (teamId t) (channelId chan)
        pPrint channel
