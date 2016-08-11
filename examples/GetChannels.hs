-- Use this module via `cabal repl` and then :load examples/GetChannels.hs
-- You will need to fill out your username, hostname, and password
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main (main) where
import qualified Data.Text as T
import           Network.Connection
import           System.Process ( readProcess )
import           Data.Aeson
import           Data.Foldable
import           Control.Monad ( when )

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

  (token, _) <- mmLogin cd login
  putStrLn ("Authenticated: " ++ show token)

  (_,Just body) <- mmGetTeams cd token
  let Success (TL teamList) = fromJSON body :: Result TeamList
  forM_ teamList $ \t -> do
    when (teamName t == team) $ do
      (hdrs,body) <- mmGetChannels cd token t
      print (teamName t)
      print (hdrs,body)

