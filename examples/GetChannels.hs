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

import           Network.Mattermost

main :: IO ()
main = do
  let user         = "<username>"
      -- Customize this command however you want, the example here assumes OSX Keychain
      passwordeval = words "security find-generic-password -s <passwordname> -w"
      team         = "<yourteam>"
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
    (token, _) <- mmLogin login
    io $ putStrLn ("Authenticated: " ++ show token)

    (_, Just body) <- mmGetTeams
    let Success (TL teamList) = fromJSON body
    forM_ teamList $ \t -> do
      when (teamName t == team) $ do
        (_hdrs,Just body) <- mmGetChannels t
        let Success (CL chans) = fromJSON body
        forM_ chans $ \chan -> do
          (_, Just body) <- mmGetChannel t chan
          io $ print body
