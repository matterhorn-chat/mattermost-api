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
import           System.Environment ( getArgs )
import           System.Exit ( exitFailure )

import           Network.Mattermost
import           Network.Mattermost.Util

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 5) $ do
    putStrLn "usage: mm-get-posts <username> <password> <team> <channel name> <host>"
    exitFailure

  let [user,pass,team,channel,host] = args
      port = 443

  ctx <- initConnectionContext
  let cd = mkConnectionData host port ctx

  let login = Login { username = T.pack user
                    , password = T.pack pass
                    , teamname = T.pack team }

  void $ runMM cd $ do
    void (mmLogin login)
    io $ putStrLn "Authenticated."

    MMResult { mmPayload = TL teamList } <- mmGetTeams
    forM_ teamList $ \t -> do
      when (teamName t == team) $ do
        MMResult { mmPayload = userMap }  <- mmGetProfiles t
        MMResult { mmPayload = CL chans } <- mmGetChannels t
        forM_ chans $ \chan -> do
          when (channelName chan == channel) $ do
            MMResult { mmPayload = posts } <- mmGetPosts t chan 0 10
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
