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
import           Control.Monad ( when )

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

  result <- runMM cd $ do
    mmUser <- mmLogin login
    io $ putStrLn "Authenticated as:"
    io $ pPrint mmUser

    teamMap <- mmGetTeams
    forM_ (HM.elems teamMap) $ \t -> do
      when (teamName t == T.unpack (configTeam config)) $ do
        userMap <- mmGetProfiles (getId t)
        Channels chans _md <- mmGetChannels (getId t)
        forM_ chans $ \chan -> do
          when (channelName chan == channel) $ do
            posts <- mmGetPosts (getId t) (getId chan) 0 10
            -- XXX: is the order really reversed?
            forM_ (reverse (postsOrder posts)) $ \postId -> do
              p    <- noteT "lookup post by id"           $ HM.lookup postId         (postsPosts posts)
              user <- noteT "lookup user using post data" $ HM.lookup (postUserId p) userMap
              let message = printf "%s: %s"
                                   (userProfileUsername user)
                                   (postMessage p)
              io $ putStrLn message
  case result of
    Left err -> putStrLn err
    Right _  -> return ()
