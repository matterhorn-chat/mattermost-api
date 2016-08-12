-- Use this module via `cabal repl` and then :load examples/GetPosts.hs
-- You will need to fill out your username, hostname, teamname, channelname, and password
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main (main) where
import           Text.Printf ( printf )
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
-- import           Text.Show.Pretty ( ppShow )
import           Network.Connection
import           System.Process ( readProcess )
import           Data.Aeson
import           Data.Foldable
import           Control.Monad ( void, when )

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

  void $ runMM cd $ do
    void (mmLogin login)
    io $ putStrLn "Authenticated."

    (_, body)   <- mmGetTeams
    rawTeamList <- noteT "get teams" body
    TL teamList <- hoistA (fromJSON rawTeamList)
    forM_ teamList $ \t -> do
      when (teamName t == team) $ do
        (_,body)    <- mmGetProfiles t
        rawProfiles <- noteT "get profiles" body
        userMap     <- hoistA (fromJSON rawProfiles)
        (_,body)    <- mmGetChannels t
        rawChannels <- noteT "get channels" body
        CL chans    <- hoistA (fromJSON rawChannels)
        forM_ chans $ \chan -> do
          when (channelName chan == channel) $ do
            (_,body) <- mmGetPosts t chan 0 10
            rawPosts <- noteT "get posts" body
            posts    <- hoistA (fromJSON rawPosts)
            -- XXX: is the order really reversed?
            forM_ (reverse (postsOrder posts)) $ \postId -> do
              p    <- noteT "lookup post by id" $
                        HM.lookup postId (postsPosts posts)
              user <- noteT "lookup user using post data" $
                        HM.lookup (postUserId p) userMap
              let message = printf "%s: %s"
                                   (userProfileUsername user)
                                   (postMessage p)
              io $ putStrLn message

