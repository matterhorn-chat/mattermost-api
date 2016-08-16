-- Note: See LocalConfig.hs to configure example for your server
module Main (main) where
import           Control.Monad ( void )
import qualified Data.Text as T
import           Network.Connection
import           System.Process ( readProcess )

import           Network.Mattermost

import           Config
import           LocalConfig -- You will need to define a function:
                             -- getConfig :: IO Config
                             -- See Config.hs

main :: IO ()
main = do
  config <- getConfig -- see LocalConfig import
  ctx    <- initConnectionContext
  let cd = mkConnectionData (T.unpack (configHostname config))
                            (fromIntegral (configPort config))
                            ctx

  let login = Login { username = configUsername config
                    , password = configPassword config
                    , teamname = configTeam     config }

  result <- runMM cd $ do
    mmUser <- mmLogin login
    io $ putStrLn "Authenticated as: "
    io $ print mmUser
    r <- mmGetTeams
    io $ print r
  case result of
    Left err -> putStrLn err
    _        -> return ()
