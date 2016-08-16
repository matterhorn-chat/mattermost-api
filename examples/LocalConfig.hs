module LocalConfig where
import           Config
import           System.Process ( readProcess )
import qualified Data.Text as T

getConfig :: IO Config
getConfig = do
  -- This example command uses the OSX keychain tool
  let cmd = words "security find-generic-password -s <passwordname> -w"
  pass <- takeWhile (/='\n') <$> readProcess (head cmd) (tail cmd) ""
  return $ Config
         { configUsername = T.pack "<username>"
         , configHostname = T.pack "<mattermost.yourserver.com>"
         , configTeam     = T.pack "<yourteam>"
         , configPort     = 443 -- currently we only support HTTPS
         , configPassword = T.pack pass
         }
