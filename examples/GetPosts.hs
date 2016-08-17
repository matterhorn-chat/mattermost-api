{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- Note: See LocalConfig.hs to configure example for your server
module Main (main) where
import           Text.Printf ( printf )
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Text.Show.Pretty ( pPrint )
import           Network.Connection
import           System.Process ( readProcess )
import           Data.Foldable
import           Control.Monad ( when, join )

import           Network.Mattermost
import           Network.Mattermost.Util

import           Config
import           LocalConfig -- You will need to define a function:
                             -- getConfig :: IO Config
                             -- See Config.hs

main :: IO ()
main = do
  config <- getConfig -- see LocalConfig import
  ctx    <- initConnectionContext
  let cd      = mkConnectionData (T.unpack (configHostname config))
                                 (fromIntegral (configPort config))
                                 ctx
      channel = "town-square"
      login   = Login { username = configUsername config
                      , password = configPassword config
                      , teamname = configTeam     config }

  (token, mmUser) <- join (hoistE <$> mmLogin cd login)
  putStrLn "Authenticated as:"
  pPrint mmUser

  teamMap <- mmGetTeams cd token
  forM_ (HM.elems teamMap) $ \t -> do
    when (teamName t == T.unpack (configTeam config)) $ do
      userMap <- mmGetProfiles cd token (getId t)
      Channels chans _md <- mmGetChannels cd token (getId t)
      forM_ chans $ \chan -> do
        when (channelName chan == channel) $ do
          posts <- mmGetPosts cd token (getId t) (getId chan) 0 10
          -- XXX: is the order really reversed?
          forM_ (reverse (postsOrder posts)) $ \postId -> do
            -- this is just a toy program, so we don't care about
            -- this pattern match failure
            let Just p    = HM.lookup postId (postsPosts posts)
                Just user = HM.lookup (postUserId p) userMap
            let message = printf "%s: %s"
                                 (userProfileUsername user)
                                 (postMessage p)
            putStrLn message
