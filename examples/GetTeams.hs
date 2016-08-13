-- Use this module via `cabal repl` and then :load examples/GetTeams.hs
-- You will need to fill out your username, hostname, and password
module Main (main) where
import           Control.Monad ( void, when )
import qualified Data.Text as T
import           Network.Connection
import           System.Process ( readProcess )
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
    r <- mmGetTeams
    io $ print r
