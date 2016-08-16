-- Use this module via `cabal repl` and then :load examples/GetTeams.hs
-- You will need to fill out your username, hostname, and password
module Main (main) where
import           Control.Monad ( void )
import qualified Data.Text as T
import           Network.Connection
import           System.Process ( readProcess )

import           Network.Mattermost

main :: IO ()
main = do
  let user         = "<your username>"
      -- Customize this command however you want, the example here assumes OSX Keychain
      passwordeval = words "security find-generic-password -s <passwordname> -w"
      team         = "<your teamname>"
      host         = "mattermost.yourserverhere.com"
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
    io $ putStrLn "Authenticated as: "
    io $ print mmUser
    r <- mmGetTeams
    io $ print r
  case result of
    Left err -> putStrLn err
    _        -> return ()
