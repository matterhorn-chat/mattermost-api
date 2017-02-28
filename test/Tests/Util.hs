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

  -- * Testing Websocket Events
  , expectWSEvent
  , expectWSEmpty

  -- * Websocket Event Predicates
  , hasWSEventType
  , forUser
  , isStatusChange
  , isPost
  , isNewUserEvent
  , wsHas
  , (&&&)
  )
where

import qualified Data.Aeson as A
import qualified Control.Exception as E
import qualified Control.Concurrent.STM as STM
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Data.Monoid ((<>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseSteps)
import Control.Monad.State.Lazy
import System.Timeout (timeout)

import Network.Mattermost
import Network.Mattermost.WebSocket
import Network.Mattermost.WebSocket.Types
import Network.Mattermost.Exceptions

import Tests.Types

mmTestCase :: String -> Config -> TestM () -> TestTree
mmTestCase testName cfg act =
    testCaseSteps testName $ \prnt -> do
      cd <- connectFromConfig cfg
      wsChan <- STM.atomically STM.newTChan
      mv <- newEmptyMVar
      let initState = TestState { tsPrinter = prnt
                                , tsConfig = cfg
                                , tsConnectionData = cd
                                , tsToken = Nothing
                                , tsDebug = True
                                , tsWebsocketChan = wsChan
                                , tsDone = mv
                                }
      (reportJSONExceptions $ evalStateT act initState) `E.finally`
        (putMVar mv ())

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

createAdminAccount :: TestM User
createAdminAccount = do
  cd <- getConnection
  cfg <- gets tsConfig
  u <- liftIO $ mmUsersCreate cd $ adminAccount cfg
  print_ "Admin Account created"
  return u

loginAccount :: Login -> TestM ()
loginAccount login = do
  cd <- getConnection
  (token, _mmUser) <- liftIO $ join (hoistE <$> mmLogin cd login)
  print_ $ "Authenticated as " ++ T.unpack (username login)
  chan <- gets tsWebsocketChan
  doneMVar <- gets tsDone
  void $ liftIO $ forkIO $ mmWithWebSocket cd token
                           (STM.atomically . STM.writeTChan chan)
                           (const $ takeMVar doneMVar)
  modify $ \ts -> ts { tsToken = Just token }

hasWSEventType :: WebsocketEventType -> WebsocketEvent -> Bool
hasWSEventType = wsHas weEvent

wsHas :: (Eq a) => (WebsocketEvent -> a) -> a -> WebsocketEvent -> Bool
wsHas f expected e = f e == expected

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) f g a = f a && g a

-- | Expect the websocket event channel to contain an event that matches
-- the specified predicate.
expectWSEvent :: String
              -- ^ A human-readable label for this test in case it
              -- fails.
              -> (WebsocketEvent -> Bool)
              -- ^ The predicate to apply.
              -> TestM ()
expectWSEvent name match = do
    chan <- gets tsWebsocketChan
    let timeoutAmount = 10 * 1000 * 1000
    mEv <- liftIO $ timeout timeoutAmount $
                   STM.atomically $ STM.readTChan chan

    case mEv of
        Nothing -> do
            let msg = "Expected a websocket event for " <> show name <>
                      " but timed out waiting"
            print_ msg
            error msg
        Just ev -> when (not $ match ev) $ do
            let msg = "Expected a websocket event for " <> show name <>
                      " but got " <> show ev
            print_ msg
            error msg

-- | Does the websocket correspond to the specified user?
forUser :: User -> WebsocketEvent -> Bool
forUser u =
    wsHas (wepUserId . weData) (Just $ userId u)

-- | Is this websocket event a status change message?
isStatusChange :: User
               -- ^ The user whose status changed
               -> T.Text
               -- ^ The new status
               -> WebsocketEvent
               -> Bool
isStatusChange u s =
    hasWSEventType WMStatusChange &&&
    forUser u &&&
    wsHas (wepStatus . weData) (Just s)

-- | Is the websocket event indicating that a new user was added to the
-- server?
isNewUserEvent :: User
               -- ^ The user that was added
               -> WebsocketEvent
               -> Bool
isNewUserEvent u =
    hasWSEventType WMNewUser &&& forUser u

-- | Is the websocket event indicating that a new message was posted to
-- a channel?
isPost :: User
       -- ^ The user who posted
       -> Channel
       -- ^ The channel to which the new post was added
       -> T.Text
       -- ^ The content of the new post
       -> WebsocketEvent
       -> Bool
isPost u ch msg =
    hasWSEventType WMPosted &&&
    wsHas (\e -> postMessage <$> (wepPost $ weData e))
          (Just msg) &&&
    wsHas (\e -> postChannelId <$> (wepPost $ weData e))
          (Just $ channelId ch) &&&
    wsHas (\e -> postUserId =<< (wepPost $ weData e))
          (Just $ userId u)

-- | Timeout in seconds for expectWSEmpty to wait before concluding that
-- no new websocket events are available.
emptyWSTimeout :: Int
emptyWSTimeout = 2

-- | Expect that the websocket event channel is empty. Waits up to
-- emptyWSTimeout seconds. Succeeds if no events are received; fails
-- otherwise.
expectWSEmpty :: TestM ()
expectWSEmpty = do
    chan <- gets tsWebsocketChan
    let timeoutAmount = emptyWSTimeout * 1000 * 1000
    mEv <- liftIO $ timeout timeoutAmount $
                   STM.atomically $ STM.readTChan chan
    case mEv of
        Nothing -> return ()
        Just ev -> do
            let msg = "Expected no websocket events but got " <> show ev
            print_ msg
            error msg

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
