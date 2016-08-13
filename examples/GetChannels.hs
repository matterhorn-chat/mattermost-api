-- Use this module via `cabal repl` and then :load examples/GetChannels.hs
-- You will need to fill out your username, hostname, and password
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main (main) where
import qualified Data.Text as T
import           Network.Connection
import           System.Process ( readProcess )
import           Data.Aeson
import           Data.Foldable
import           Control.Monad ( void, when )
import           System.Environment ( getArgs )
import           System.Exit ( exitFailure )

import           Network.Mattermost

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
    MMResult { mmPayload = token } <- mmLogin login
    io $ putStrLn ("Authenticated: " ++ show token)
    MMResult { mmPayload = TL teamList } <- mmGetTeams
    forM_ teamList $ \t -> do
      when (teamName t == team) $ do
        MMResult { mmPayload = CL chans } <- mmGetChannels t
        forM_ chans $ \chan -> do
          cl <- mmGetChannel t chan
          io $ print (mmJSONBody cl)
