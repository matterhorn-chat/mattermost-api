{-# LANGUAGE OverloadedStrings #-}
module Main (
  main
) where

import           Data.Text (Text)
import           Data.Monoid ((<>))

import           Control.Exception

import           System.Exit

import           Text.Show.Pretty ( ppShow )

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq

import           Test.Tasty
import           Test.Tasty.HUnit

import           Network.Mattermost
import           Network.Mattermost.Exceptions

import           Tests.Util
import           Tests.Types

main :: IO ()
main = defaultMain tests `catch` \(JSONDecodeException msg badJson) -> do
  putStrLn $ "JSONDecodeException: " ++ msg
  putStrLn badJson
  exitFailure

-- Users and other test configuration data

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

testAccount :: UsersCreate
testAccount =
    UsersCreate { usersCreateEmail          = "test-user@example.com"
                , usersCreatePassword       = password testUserLogin
                , usersCreateUsername       = username testUserLogin
                , usersCreateAllowMarketing = False
                }

-- Test groups

tests :: TestTree
tests = testGroup "Tests"
    [ setup
    , unitTests
    ]

-- Note that the order of the tests matters as each may have side
-- effects on which subsequent tests depend.
unitTests :: TestTree
unitTests = testGroup "Units"
    [ loginAsNormalUserTest
    , initialLoadTest
    , createChannelTest
    , getChannelsTest
    , leaveChannelTest
    , joinChannelTest
    ]

-- Test definitions

setup :: TestTree
setup = testCaseSteps "Setup" $ \prnt -> reportJSONExceptions $ do
  whenDebug $ prnt "Creating connection"
  let cfg = testConfig

  cd <- initConnectionDataInsecure (configHostname cfg)
                                   (fromIntegral (configPort cfg))
  -- XXX: Use something like this if you want logging (useful when debugging)
  -- let cd = cd' `withLogger` mmLoggerDebugErr

  whenDebug $ prnt "Creating Admin account"
  _adminUser <- createAdminAccount cfg cd prnt
  whenDebug $ prnt "Logging into Admin account"
  adminToken <- loginAdminAccount cfg cd prnt

  whenDebug $ prnt "Creating test team"
  testTeam <- createTeam cd adminToken testTeamsCreate prnt

  whenDebug $ prnt "Getting Config"
  config <- mmGetConfig cd adminToken
  whenDebug $ prnt (ppShow config)

  whenDebug $ prnt "Saving Config"
  -- Enable open team so that the admin can create
  -- new users.
  let Object oldConfig    = config
      Object teamSettings = oldConfig HM.! "TeamSettings"
      newConfig           = Object (HM.insert "TeamSettings"
                                   (Object (HM.insert "EnableOpenServer"
                                           (Bool True) teamSettings)) oldConfig)
  mmSaveConfig cd adminToken newConfig

  whenDebug $ prnt "Creating test account"
  testUser   <- createAccount cd adminToken testAccount prnt

  whenDebug $ prnt "Add test user to test team"
  mmTeamAddUser cd adminToken (teamId testTeam) (userId testUser)

loginAsNormalUserTest :: TestTree
loginAsNormalUserTest = testCaseSteps "Logging to normal account" $ \prnt ->
  reportJSONExceptions $ do
    let cfg = testConfig
    cd <- initConnectionDataInsecure (configHostname cfg)
                                     (fromIntegral (configPort cfg))
    _userToken <- loginAccount cd testUserLogin prnt
    return ()

initialLoadTest :: TestTree
initialLoadTest = testCaseSteps "Initial Load" $ \prnt -> reportJSONExceptions $ do
  let cfg = testConfig
  cd <- initConnectionDataInsecure (configHostname cfg)
                                   (fromIntegral (configPort cfg))
  userToken   <- loginAccount cd testUserLogin prnt
  initialLoad <- mmGetInitialLoad cd userToken
  -- print the team names
  whenDebug $ prnt (ppShow (fmap teamName (initialLoadTeams initialLoad)))

createChannelTest :: TestTree
createChannelTest = testCaseSteps "Create Channel" $ \prnt -> reportJSONExceptions $ do
  let cfg = testConfig
  cd <- initConnectionDataInsecure (configHostname cfg)
                                   (fromIntegral (configPort cfg))
  userToken   <- loginAccount cd testUserLogin prnt
  initialLoad <- mmGetInitialLoad cd userToken
  let team Seq.:< _ = Seq.viewl (initialLoadTeams initialLoad)
  chan <- mmCreateChannel cd userToken (teamId team) testMinChannel
  whenDebug $ prnt (ppShow chan)

getChannelsTest :: TestTree
getChannelsTest = testCaseSteps "Get Channels" $ \prnt -> reportJSONExceptions $ do
  let cfg = testConfig
  cd <- initConnectionDataInsecure (configHostname cfg)
                                   (fromIntegral (configPort cfg))
  userToken   <- loginAccount cd testUserLogin prnt
  initialLoad <- mmGetInitialLoad cd userToken
  let team Seq.:< _ = Seq.viewl (initialLoadTeams initialLoad)
  chans <- mmGetChannels cd userToken (teamId team)
  let chan Seq.:< _ = Seq.viewl chans
  whenDebug $ prnt (ppShow chan)

findChannel :: Channels -> Text -> Channel
findChannel chans name =
    let result = Seq.viewl (Seq.filter (\c -> channelName c == name) chans)
    in case result of
        chan Seq.:< _ -> chan
        _ -> error $ "Expected to find channel by name " <> show name

leaveChannelTest :: TestTree
leaveChannelTest = testCaseSteps "Leave Channel" $ \prnt -> reportJSONExceptions $ do
  let cfg = testConfig
  cd <- initConnectionDataInsecure (configHostname cfg)
                                   (fromIntegral (configPort cfg))
  userToken   <- loginAccount cd testUserLogin prnt
  initialLoad <- mmGetInitialLoad cd userToken
  let team Seq.:< _ = Seq.viewl (initialLoadTeams initialLoad)
  chans <- mmGetChannels cd userToken (teamId team)
  whenDebug $ prnt (ppShow chans)
  let chan = findChannel chans $ minChannelName testMinChannel
  mmLeaveChannel cd userToken (teamId team) (channelId chan)

joinChannelTest :: TestTree
joinChannelTest = testCaseSteps "Join Channel" $ \prnt -> reportJSONExceptions $ do
  let cfg = testConfig
  cd <- initConnectionDataInsecure (configHostname cfg)
                                   (fromIntegral (configPort cfg))
  userToken   <- loginAccount cd testUserLogin prnt
  initialLoad <- mmGetInitialLoad cd userToken
  let team Seq.:< _ = Seq.viewl (initialLoadTeams initialLoad)
  chans <- mmGetMoreChannels cd userToken (teamId team)
  whenDebug $ prnt (ppShow chans)
  let chan = findChannel chans $ minChannelName testMinChannel
  mmJoinChannel cd userToken (teamId team) (channelId chan)
