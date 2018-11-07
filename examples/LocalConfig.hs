{-# LANGUAGE OverloadedStrings #-}
module LocalConfig where
import           Config
import           System.Process ( readProcess )
import qualified Data.Text as T
import           Network.Mattermost.Types ( UserText (..) )

getConfig :: IO Config
getConfig = do
  -- This example command uses the OSX keychain tool
  let cmd = words "pass ldap"
  pass <- takeWhile (/='\n') <$> readProcess (head cmd) (tail cmd) ""
  return $ Config
         { configUsername = "USERNAME"
         , configHostname = "mattermost.example.com"
         , configTeam     = UserText "TEAMNAME"
         , configPort     = 443 -- currently we only support HTTPS
         , configPassword = T.pack pass
         }
