module LocalConfig where
import           Config
import           System.Process ( readProcess )
import qualified Data.Text as T

getConfig :: IO Config
getConfig = do
  -- This example command uses the OSX keychain tool
  let cmd = words "pass ldap"
  pass <- takeWhile (/='\n') <$> readProcess (head cmd) (tail cmd) ""
  return $ Config
         { configUsername = T.pack "gdritter"
         , configHostname = T.pack "mattermost.galois.com"
         , configTeam     = T.pack "galwegians"
         , configPort     = 443 -- currently we only support HTTPS
         , configPassword = T.pack pass
         }
