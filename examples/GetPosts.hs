-- Use this module via `cabal repl` and then :load examples/GetPosts.hs
-- You will need to fill out your username, hostname, teamname, channelname, and password
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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

main :: IO ()
main = do
  let user         = "<username>"
      -- Customize this command however you want, the example here assumes OSX Keychain
      passwordeval = words "security find-generic-password -s <passwordname> -w"
      team         = "<teamname>"
      channel      = "<channelname>"
      host         = "mattermost.yourserver.com"
      port         = 443 -- only supports https at the moment

  ctx <- initConnectionContext
  let cd = mkConnectionData host port ctx

  -- XXX: this is a hack to drop the trailing newline
  pass <- head . lines <$> readProcess (head passwordeval) (tail passwordeval) ""
  let login = Login { username = T.pack user
                    , password = T.pack pass
                    , teamname = T.pack team }

  result <- runMM cd $ do
    mmUser <- mmLogin login
    io $ putStrLn "Authenticated as:"
    io $ pPrint mmUser

    teamMap <- mmGetTeams
    forM_ (HM.elems teamMap) $ \t -> do
      when (teamName t == team) $ do
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
