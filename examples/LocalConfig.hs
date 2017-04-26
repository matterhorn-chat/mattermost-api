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
         { configUsername = T.pack "USERNAME"
         , configHostname = T.pack "mattermost.example.com"
         , configTeam     = T.pack "TEAMNAME"
         , configPort     = 443 -- currently we only support HTTPS
         , configPassword = T.pack pass
         }
