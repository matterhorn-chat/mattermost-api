{-# LANGUAGE OverloadedStrings #-}
module Main (
  main
) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Control.Exception
import           Control.Monad ( join, void )

import           System.Exit

import           Text.Show.Pretty ( ppShow )

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq

import           Test.Tasty
import           Test.Tasty.HUnit

import           Network.Mattermost
import           Network.Mattermost.Exceptions

main :: IO ()
main = defaultMain tests `catch` \(JSONDecodeException msg badJson) -> do
  putStrLn $ "JSONDecodeException: " ++ msg
  putStrLn badJson
  exitFailure

-- Users and other test configuration data

data Config
  = Config
  { configUsername :: Text
  , configHostname :: Text
  , configTeam     :: Text
  , configPort     :: Int
  , configPassword :: Text
  , configEmail    :: Text
  }

testConfig :: Config
testConfig = Config
  { configUsername = "testAdmin"
  , configEmail    = "testAdmin@example.com"
  , configHostname = "localhost"
  , configTeam     = "testteam"
  , configPort     = 8065
  , configPassword = "password"
  }

testUserLogin :: Login
testUserLogin = Login
  { username = "test-user"
  , password = "password"
  }

testMinChannel :: MinChannel
testMinChannel = MinChannel
  { minChannelName        = "test-channel"
  , minChannelDisplayName = "Test Channel"
  , minChannelPurpose     = Just "A channel for test cases"
  , minChannelHeader      = Just "Test Header"
  , minChannelType        = Ordinary
  }

testTeamsCreate :: TeamsCreate
testTeamsCreate = TeamsCreate
  { teamsCreateDisplayName = "Test Team"
  , teamsCreateName        = "testteam"
  , teamsCreateType        = Ordinary
  }

-- Test groups

tests :: TestTree
tests = testGroup "Tests"
    [ setup
    , unitTests
    ]

unitTests :: TestTree
unitTests = testGroup "Units"
    [ loginAsNormalUserTest
    , initialLoadTest
    , createChannelTest
    , getChannelsTest
    , leaveChannelTest
    , joinChannelTest
    ]

-- This only exists because tasty will call `show` on the exception that we give
-- it. If we directly output the exception first then we avoid an unnecessary
-- level of quotation in the output. We still throw the exception though so that
-- tasty reports the correct exception type. This results in some redundancy but
-- we only see it when there are failures, so it seems acceptable.
catchAndPrintJSONDecodeException :: IO a -> IO a
catchAndPrintJSONDecodeException io = io
  `catch` \e@(JSONDecodeException msg badJson) -> do
  putStrLn $ "\nException: JSONDecodeException: " ++ msg
  putStrLn badJson
  throwIO e

-- Test definitions

setup :: TestTree
setup = testCaseSteps "Setup" $ \prnt -> catchAndPrintJSONDecodeException $ do
  prnt "Creating connection"
  cd <- initConnectionDataInsecure (T.unpack (configHostname testConfig))
                                   (fromIntegral (configPort testConfig))
  -- XXX: Use something like this if you want logging (useful when debugging)
  -- let cd = cd' `withLogger` mmLoggerDebugErr

  prnt "Creating Admin account"
  _adminUser <- createAdminAccount cd prnt
  prnt "Logging into Admin account"
  adminToken <- loginAdminAccount cd prnt

  prnt "Creating test team"
  testTeam <- createTestTeam cd adminToken prnt

  prnt "Getting Config"
  config <- mmGetConfig cd adminToken
  -- prnt (ppShow config)

  prnt "Saving Config"
  -- Enable open team so that the admin can create
  -- new users.
  let Object oldConfig    = config
      Object teamSettings = oldConfig HM.! "TeamSettings"
      newConfig           = Object (HM.insert "TeamSettings"
                                   (Object (HM.insert "EnableOpenServer"
                                           (Bool True) teamSettings)) oldConfig)
  mmSaveConfig cd adminToken newConfig

  prnt "Creating test account"
  testUser   <- createTestAccount cd adminToken prnt

  prnt "Add test user to test team"
  mmTeamAddUser cd adminToken (teamId testTeam) (userId testUser)

loginAsNormalUserTest :: TestTree
loginAsNormalUserTest = testCaseSteps "Logging to normal account" $ \prnt ->
  catchAndPrintJSONDecodeException $ do
    cd <- initConnectionDataInsecure (T.unpack (configHostname testConfig))
                                     (fromIntegral (configPort testConfig))
    _userToken <- loginAccount cd testUserLogin prnt
    return ()

initialLoadTest :: TestTree
initialLoadTest = testCaseSteps "Initial Load" $ \prnt -> catchAndPrintJSONDecodeException $ do
  cd <- initConnectionDataInsecure (T.unpack (configHostname testConfig))
                                   (fromIntegral (configPort testConfig))
  userToken   <- loginAccount cd testUserLogin prnt
  initialLoad <- mmGetInitialLoad cd userToken
  -- print the team names
  prnt (ppShow (fmap teamName (initialLoadTeams initialLoad)))

createChannelTest :: TestTree
createChannelTest = testCaseSteps "Create Channel" $ \prnt -> catchAndPrintJSONDecodeException $ do
  cd <- initConnectionDataInsecure (T.unpack (configHostname testConfig))
                                   (fromIntegral (configPort testConfig))
  userToken   <- loginAccount cd testUserLogin prnt
  initialLoad <- mmGetInitialLoad cd userToken
  let team Seq.:< _ = Seq.viewl (initialLoadTeams initialLoad)
  chan <- mmCreateChannel cd userToken (teamId team) testMinChannel
  prnt (ppShow chan)
  return ()

getChannelsTest :: TestTree
getChannelsTest = testCaseSteps "Get Channels" $ \prnt -> catchAndPrintJSONDecodeException $ do
  cd <- initConnectionDataInsecure (T.unpack (configHostname testConfig))
                                   (fromIntegral (configPort testConfig))
  userToken   <- loginAccount cd testUserLogin prnt
  initialLoad <- mmGetInitialLoad cd userToken
  let team Seq.:< _ = Seq.viewl (initialLoadTeams initialLoad)
  chans <- mmGetChannels cd userToken (teamId team)
  let chan Seq.:< _ = Seq.viewl chans
  prnt (ppShow chan)

leaveChannelTest :: TestTree
leaveChannelTest = testCaseSteps "Leave Channel" $ \prnt -> catchAndPrintJSONDecodeException $ do
  cd <- initConnectionDataInsecure (T.unpack (configHostname testConfig))
                                   (fromIntegral (configPort testConfig))
  userToken   <- loginAccount cd testUserLogin prnt
  initialLoad <- mmGetInitialLoad cd userToken
  let team Seq.:< _ = Seq.viewl (initialLoadTeams initialLoad)
  chans <- mmGetChannels cd userToken (teamId team)
  prnt (ppShow chans)
  let chan Seq.:< _ = Seq.viewl
                        (Seq.filter (\c -> channelName c == minChannelName testMinChannel)
                                    chans)
  mmLeaveChannel cd userToken (teamId team) (channelId chan)

joinChannelTest :: TestTree
joinChannelTest = testCaseSteps "Join Channel" $ \prnt -> catchAndPrintJSONDecodeException $ do
  cd <- initConnectionDataInsecure (T.unpack (configHostname testConfig))
                                   (fromIntegral (configPort testConfig))
  userToken   <- loginAccount cd testUserLogin prnt
  initialLoad <- mmGetInitialLoad cd userToken
  let team Seq.:< _ = Seq.viewl (initialLoadTeams initialLoad)
  chans <- mmGetMoreChannels cd userToken (teamId team)
  prnt (ppShow chans)
  let chan Seq.:< _ = Seq.viewl
                        (Seq.filter (\c -> channelName c == minChannelName testMinChannel)
                                    chans)
  mmJoinChannel cd userToken (teamId team) (channelId chan)

-- Wrapper functions used in test cases

adminAccount :: UsersCreate
adminAccount =
    UsersCreate { usersCreateEmail          = configEmail    testConfig
                , usersCreatePassword       = configPassword testConfig
                , usersCreateUsername       = configUsername testConfig
                , usersCreateAllowMarketing = True
                }

testAccount :: UsersCreate
testAccount =
    UsersCreate { usersCreateEmail          = "test-user@example.com"
                , usersCreatePassword       = password testUserLogin
                , usersCreateUsername       = username testUserLogin
                , usersCreateAllowMarketing = False
                }

createAdminAccount :: ConnectionData -> (String -> IO ()) -> IO ()
createAdminAccount cd prnt = do
  void $ mmUsersCreate cd adminAccount
  prnt "Admin Account created"

createTestTeam :: ConnectionData -> Token -> (String -> IO ()) -> IO Team
createTestTeam cd token prnt = do
  team <- mmCreateTeam cd token testTeamsCreate
  prnt "Test team created"
  return team

createTestAccount :: ConnectionData -> Token -> (String -> IO ()) -> IO User
createTestAccount cd token prnt = do
  newUser <- mmUsersCreateWithToken cd token testAccount
  prnt "Test Account created"
  return newUser

loginAdminAccount :: ConnectionData -> (String -> IO ()) -> IO Token
loginAdminAccount cd = loginAccount cd admin
  where
  admin = Login { username = configUsername testConfig
                , password = configPassword testConfig
                }

loginAccount :: ConnectionData -> Login -> (String -> IO ()) -> IO Token
loginAccount cd login prnt = do
  (token, _mmUser) <- join (hoistE <$> mmLogin cd login)
  prnt $ "Authenticated as " ++ T.unpack (username login)
  return token
