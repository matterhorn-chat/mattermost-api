module Tests.Util
  ( mmTestCase
  , print_
  , getConnection
  , getToken
  , getInitialLoad
  , createChannel
  , joinChannel
  , leaveChannel
  , getMoreChannels
  , getChannels
  , getConfig
  , saveConfig
  , teamAddUser
  , reportJSONExceptions
  , adminAccount
  , createAdminAccount
  , loginAccount
  , loginAdminAccount
  , createAccount
  , createTeam
  , findChannel
  , connectFromConfig
  )
where

import qualified Data.Aeson as A
import qualified Control.Exception as E
import Data.Monoid ((<>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseSteps)
import Control.Monad.State.Lazy

import Network.Mattermost
import Network.Mattermost.Exceptions

import Tests.Types

mmTestCase :: String -> Config -> TestM () -> TestTree
mmTestCase testName cfg act =
    testCaseSteps testName $ \prnt -> do
      cd <- connectFromConfig cfg
      let initState = TestState { tsPrinter = prnt
                                , tsConfig = cfg
                                , tsConnectionData = cd
                                , tsToken = Nothing
                                , tsDebug = False
                                }
      reportJSONExceptions $ evalStateT act initState

print_ :: String -> TestM ()
print_ s = do
    dbg <- gets tsDebug
    printFunc <- gets tsPrinter
    when dbg $ liftIO $ printFunc s

-- This only exists because tasty will call `show` on the exception that
-- we give it. If we directly output the exception first then we avoid
-- an unnecessary level of quotation in the output. We still throw the
-- exception though so that tasty reports the correct exception type.
-- This results in some redundancy but we only see it when there are
-- failures, so it seems acceptable.
reportJSONExceptions :: IO a -> IO a
reportJSONExceptions io = io
  `E.catch` \e@(JSONDecodeException msg badJson) -> do
  putStrLn $ "\nException: JSONDecodeException: " ++ msg
  putStrLn badJson
  E.throwIO e

adminAccount :: Config -> UsersCreate
adminAccount cfg =
    UsersCreate { usersCreateEmail          = configEmail    cfg
                , usersCreatePassword       = configPassword cfg
                , usersCreateUsername       = configUsername cfg
                , usersCreateAllowMarketing = True
                }

createAdminAccount :: TestM ()
createAdminAccount = do
  cd <- getConnection
  cfg <- gets tsConfig
  void $ liftIO $ mmUsersCreate cd $ adminAccount cfg
  print_ "Admin Account created"

loginAccount :: Login -> TestM ()
loginAccount login = do
  cd <- getConnection
  (token, _mmUser) <- liftIO $ join (hoistE <$> mmLogin cd login)
  print_ $ "Authenticated as " ++ T.unpack (username login)
  modify $ \ts -> ts { tsToken = Just token }

loginAdminAccount :: TestM ()
loginAdminAccount = do
    cfg <- gets tsConfig
    let admin = Login { username = configUsername cfg
                      , password = configPassword cfg
                      }
    loginAccount admin

createAccount :: UsersCreate -> TestM User
createAccount account = do
  cd <- getConnection
  token <- getToken
  newUser <- liftIO $ mmUsersCreateWithToken cd token account
  print_ $ "account created for " <> (T.unpack $ usersCreateUsername account)
  return newUser

createTeam :: TeamsCreate -> TestM Team
createTeam tc = do
  cd <- getConnection
  token <- getToken
  team <- liftIO $ mmCreateTeam cd token tc
  print_ $ "Team created: " <> (T.unpack $ teamsCreateName tc)
  return team

findChannel :: Channels -> T.Text -> Channel
findChannel chans name =
    let result = Seq.viewl (Seq.filter (\c -> channelName c == name) chans)
    in case result of
        chan Seq.:< _ -> chan
        _ -> error $ "Expected to find channel by name " <> show name

connectFromConfig :: Config -> IO ConnectionData
connectFromConfig cfg =
  initConnectionDataInsecure (configHostname cfg)
                             (fromIntegral (configPort cfg))

getConnection :: TestM ConnectionData
getConnection = gets tsConnectionData

getToken :: TestM Token
getToken = do
    val <- gets tsToken
    case val of
        Just tok -> return tok
        Nothing -> error "Expected authentication token but none was present"

getInitialLoad :: TestM InitialLoad
getInitialLoad = do
  cd <- getConnection
  token <- getToken
  liftIO $ mmGetInitialLoad cd token

createChannel :: Team -> MinChannel -> TestM Channel
createChannel team mc = do
  cd <- getConnection
  token <- getToken
  liftIO $ mmCreateChannel cd token (teamId team) mc

joinChannel :: Team -> Channel -> TestM ()
joinChannel team chan = do
  cd <- getConnection
  token <- getToken
  liftIO $ mmJoinChannel cd token (teamId team) (channelId chan)

getMoreChannels :: Team -> TestM Channels
getMoreChannels team = do
  cd <- getConnection
  token <- getToken
  liftIO $ mmGetMoreChannels cd token (teamId team)

leaveChannel :: Team -> Channel -> TestM ()
leaveChannel team chan = do
  cd <- getConnection
  token <- getToken
  liftIO $ mmLeaveChannel cd token (teamId team) (channelId chan)

getChannels :: Team -> TestM Channels
getChannels team = do
  cd <- getConnection
  token <- getToken
  liftIO $ mmGetChannels cd token (teamId team)

getConfig :: TestM A.Value
getConfig = do
  cd <- getConnection
  token <- getToken
  liftIO $ mmGetConfig cd token

saveConfig :: A.Value -> TestM ()
saveConfig newConfig = do
  cd <- getConnection
  token <- getToken
  liftIO $ mmSaveConfig cd token newConfig

teamAddUser :: Team -> User -> TestM ()
teamAddUser team user = do
  cd <- getConnection
  token <- getToken
  liftIO $ mmTeamAddUser cd token (teamId team) (userId user)
