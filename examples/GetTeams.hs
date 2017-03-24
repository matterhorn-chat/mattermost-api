-- Note: See LocalConfig.hs to configure example for your server
module Main (main) where
import           Control.Monad ( void, join )
import qualified Data.Text as T
import           Text.Show.Pretty ( pPrint )
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
  let cd = mkConnectionData (configHostname config)
                            (fromIntegral (configPort config))
                            ctx
  let login = Login { username = configUsername config
                    , password = configPassword config
                    }

  (session, mmUser) <- join (hoistE <$> mmLogin cd login)
  putStrLn "Authenticated as: "
  pPrint mmUser
  i <- mmGetInitialLoad session
  pPrint $ initialLoadTeams i
