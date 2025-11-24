module Tests.Util
  ( mmTestCase
  , print_
  , getConnection
  , getSession
  , getTeams
  , createChannel
  , deleteChannel
  , joinChannel
  , leaveChannel
--  , getMoreChannels
  , getChannels
  , getChannelMembers
  , getUserByName
  , getConfig
  , getClientConfig
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
  , getMe

  -- * Testing Websocket Events
  , expectWSEvent
  , expectWSDone

  -- * Websocket Event Predicates
  , hasWSEventType
  , forUser
  , forChannel
  , isStatusChange
  , isPost
  , isNewUserEvent
  , isChannelCreatedEvent
  , isChannelDeleteEvent
  , isAddedToTeam
  , isUserJoin
  , isUserLeave
  , isViewedChannel
  , wsHas
  , (&&&)
  )
where

import qualified Control.Exception as E
import qualified Control.Concurrent.STM as STM
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (join, when)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Foldable as F
import Data.Functor (void)
import Data.Monoid ((<>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseSteps)
import Control.Monad.State.Lazy (evalStateT, gets, modify)
import System.Timeout (timeout)

import Network.Mattermost.Endpoints
import Network.Mattermost.Types
import Network.Mattermost.Types.Config
import Network.Mattermost.WebSocket
import Network.Mattermost.Exceptions
import Network.Mattermost.Util

import Tests.Types

mmTestCase :: String -> TestConfig -> TestM () -> TestTree
mmTestCase testName cfg act =
    testCaseSteps testName $ \prnt -> do
      cd <- connectFromConfig cfg
      wsChan <- STM.atomically STM.newTChan
      mv <- newEmptyMVar
      let initState = TestState { tsPrinter = prnt
                                , tsConfig = cfg
                                , tsConnectionData = cd
                                , tsSession = Nothing
                                , tsDebug = False
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

adminAccount :: TestConfig -> UsersCreate
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
  u <- liftIO $ mmInitialUser cd $ adminAccount cfg
  print_ "Admin Account created"
  return u

loginAccount :: Login -> TestM ()
loginAccount login = do
  cd <- getConnection
  (session, _mmUser) <- liftIO $ join (hoistE <$> mmLogin cd login)
  print_ $ "Authenticated as " ++ T.unpack (username login)
  chan <- gets tsWebsocketChan
  doneMVar <- gets tsDone
  printFunc <- gets tsPrinter
  void $ liftIO $ forkIO $ mmWithWebSocket session
                           (either printFunc (either (\_ -> pure()) (STM.atomically . STM.writeTChan chan)))
                           (const $ takeMVar doneMVar)
  modify $ \ts -> ts { tsSession = Just session }

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

-- | Does the websocket correspond to the specified channel?
forChannel :: Channel -> WebsocketEvent -> Bool
forChannel ch =
    wsHas (wepChannelId . weData) (Just $ channelId ch)

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

isViewedChannel :: WebsocketEvent -> Bool
isViewedChannel = hasWSEventType WMChannelViewed

-- | Is the websocket event indicating that a new user was added to the
-- team
isAddedToTeam :: User
              -- ^ The user that was added
              -> Team
              -- ^ The team to which the user was added
              -> WebsocketEvent
              -> Bool
isAddedToTeam u _ =
    hasWSEventType WMAddedToTeam &&& forUser u

isChannelCreatedEvent :: Channel
                      -> WebsocketEvent
                      -> Bool
isChannelCreatedEvent c =
    hasWSEventType WMChannelCreated &&& forChannel c

-- | Is the websocket event indicating that a channel was deleted?
isChannelDeleteEvent :: Channel -> WebsocketEvent -> Bool
isChannelDeleteEvent ch =
    forChannel ch &&& hasWSEventType WMChannelDeleted

-- | Is the websocket event indicating that a user joined a channel?
isUserJoin :: User
           -- ^ The user that joined a channel
           -> Channel
           -- ^ The channel that was joined
           -> WebsocketEvent
           -> Bool
isUserJoin u ch =
    hasWSEventType WMUserAdded &&&
    forUser u &&&
    wsHas (webChannelId . weBroadcast) (Just $ channelId ch)

-- | Is the websocket event indicating that a user left a channel?
isUserLeave :: User
            -- ^ The user that left a channel
            -> Channel
            -- ^ The channel that the user left
            -> WebsocketEvent
            -> Bool
isUserLeave u ch =
    hasWSEventType WMUserRemoved &&&
    forChannel ch &&&
    wsHas (webUserId . weBroadcast) (Just $ userId u)

-- | Is the websocket event indicating that a new message was posted to
-- a channel?
isPost :: User
       -- ^ The user who posted
       -> Channel
       -- ^ The channel to which the new post was added
       -> UserText
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

-- | Timeout in seconds for expectWSDone to wait before concluding that
-- no new websocket events are available.
emptyWSTimeout :: Int
emptyWSTimeout = 2

-- | Expect that the websocket event channel is empty. Waits up to
-- emptyWSTimeout seconds. Succeeds if no events are received; fails
-- otherwise.
expectWSDone :: TestM ()
expectWSDone = do
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
                      , otpToken = Nothing
                      }
    loginAccount admin

createAccount :: UsersCreate -> TestM User
createAccount account = do
  session <- getSession
  newUser <- liftIO $ mmCreateUser account session
  print_ $ "account created for " <> (T.unpack $ usersCreateUsername account)
  return newUser

createTeam :: TeamsCreate -> TestM Team
createTeam tc = do
  session <- getSession
  team <- liftIO $ mmCreateTeam tc session
  print_ $ "Team created: " <> (T.unpack $ teamsCreateName tc)
  return team

findChannel :: Channels -> UserText -> Channel
findChannel chans name =
    let result = Seq.viewl (Seq.filter nameMatches chans)
        nameMatches c = name `elem` [ channelName c
                                    , channelDisplayName c
                                    ]
    in case result of
        chan Seq.:< _ -> chan
        _ ->
            let namePairs = mkPair <$> chans
                mkPair c = (channelName c, channelDisplayName c)
            in error $ "Expected to find channel by name " <>
                     show name <> " but got " <> show namePairs

connectFromConfig :: TestConfig -> IO ConnectionData
connectFromConfig cfg =
  initConnectionData (configHostname cfg) (fromIntegral (configPort cfg)) T.empty ConnectHTTP
                     defaultConnectionPoolConfig

getConnection :: TestM ConnectionData
getConnection = gets tsConnectionData

getSession :: TestM Session
getSession = do
    val <- gets tsSession
    case val of
        Just s -> return s
        Nothing -> error "Expected authentication token but none was present"

getTeams :: TestM (Seq.Seq Team)
getTeams = do
  session <- getSession
  liftIO $ mmGetUsersTeams UserMe session

getMe :: TestM User
getMe = do
  session <- getSession
  liftIO $ mmGetUser UserMe session

getUserByName :: T.Text -> TestM (Maybe User)
getUserByName uname = do
    session <- getSession
    let query = defaultUserQuery
          { userQueryPage = Just 0
          , userQueryPerPage = Just 10000
          }
    allUserMap <- liftIO $ mmGetUsers query session
    -- Find the user matching the username and get its ID
    let matches = Seq.filter matchingUser allUserMap
        matchingUser u = userUsername u == uname

    case Seq.viewl matches of
        user Seq.:< _ -> do
            let uId = userId user
            -- Then load the User record
            Just <$> (liftIO $ mmGetUser (UserById uId) session)
        _ -> return Nothing

createChannel :: MinChannel -> TestM Channel
createChannel mc = do
  session <- getSession
  liftIO $ mmCreateChannel mc session

deleteChannel :: Channel -> TestM ()
deleteChannel ch = do
  session <- getSession
  liftIO $ mmDeleteChannel (channelId ch) session

joinChannel :: User -> Channel -> TestM ()
joinChannel user chan = do
  session <- getSession
  let member = MinChannelMember
        { minChannelMemberUserId = userId user
        , minChannelMemberChannelId = channelId chan
        }
  liftIO $ void $ mmAddUser (channelId chan) member session

leaveChannel :: Channel -> TestM ()
leaveChannel chan = do
  session <- getSession
  liftIO $ mmRemoveUserFromChannel (channelId chan) UserMe session

getChannelMembers :: Channel -> TestM [User]
getChannelMembers chan = do
  session <- getSession
  let query = defaultUserQuery
        { userQueryPage = Just 0
        , userQueryPerPage = Just 10000
        , userQueryInChannel = Just (channelId chan)
        }
  F.toList <$> (liftIO $ mmGetUsers query session)

getChannels :: Team -> TestM Channels
getChannels team = do
  session <- getSession
  liftIO $ mmGetPublicChannels (teamId team) Nothing Nothing session

getConfig :: TestM ServerConfig
getConfig = do
  session <- getSession
  liftIO $ mmGetConfiguration session

getClientConfig :: TestM ClientConfig -- A.Value
getClientConfig = do
  session <- getSession
  liftIO $ mmGetClientConfiguration (Just (T.pack "old")) session

saveConfig :: ServerConfig -> TestM ()
saveConfig newConfig = do
  session <- getSession
  liftIO $ void $ mmUpdateConfiguration newConfig session

teamAddUser :: Team -> User -> TestM ()
teamAddUser team user = do
  session <- getSession
  let member = TeamMember
        { teamMemberUserId = userId user
        , teamMemberTeamId = teamId team
        , teamMemberRoles  = T.empty
        }
  liftIO $ void $ mmAddUserToTeam (teamId team) member session
