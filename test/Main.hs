{-# LANGUAGE OverloadedStrings #-}
module Main (
  main
) where

import           Control.Exception
import           Control.Monad (when)

import           System.Exit

import           Text.Show.Pretty ( ppShow )

import           Data.Monoid ((<>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T (Text)

import           Test.Tasty

import           Network.Mattermost.Types
import           Network.Mattermost.Types.Config
import           Network.Mattermost.WebSocket.Types
import           Network.Mattermost.Exceptions

import           Tests.Util
import           Tests.Types

main :: IO ()
main = defaultMain tests `catch` \(JSONDecodeException msg badJson) -> do
  putStrLn $ "JSONDecodeException: " ++ msg
  putStrLn badJson
  exitFailure

-- Users and other test configuration data

testConfig :: TestConfig
testConfig = TestConfig
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

testMinChannel :: TeamId -> MinChannel
testMinChannel tId = MinChannel
  { minChannelName        = "test-channel"
  , minChannelDisplayName = "Test Channel"
  , minChannelPurpose     = Just "A channel for test cases"
  , minChannelHeader      = Just "Test Header"
  , minChannelType        = Ordinary
  , minChannelTeamId      = tId
  }

testMinChannelName :: T.Text
testMinChannelName = "test-channel"


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
    , deleteChannelTest
    , clientConfigTest
    ]

-- Test definitions

setup :: TestTree
setup = mmTestCase "Setup" testConfig $ do
  adminUser <- createAdminAccount

  print_ "Logging into Admin account"
  loginAdminAccount

  expectWSEvent "hello" (hasWSEventType WMHello)
  expectWSEvent "status" (isStatusChange adminUser "online")

  print_ "Creating test team"
  testTeam <- createTeam testTeamsCreate

  -- Load channels so we can get the IDs of joined channels
  chans <- getChannels testTeam

  let townSquare = findChannel chans "Town Square"
      offTopic   = findChannel chans "Off-Topic"

  print_ "Getting Config"
  config <- getConfig

  print_ "Saving Config"
  -- Enable open team so that the admin can create
  -- new users.
  let newTeamSettings = (configTeamsettings config) { teamSettingsEnableopenserver = True }
      newConfig = config { configTeamsettings = newTeamSettings }
  saveConfig newConfig

  expectWSEvent "admin joined town square"
    (isPost adminUser townSquare "testadmin has joined the channel.")

  expectWSEvent "admin joined test team"
    (isAddedToTeam adminUser testTeam)

  print_ "Creating test account"
  testUser <- createAccount testAccount

  print_ "Add test user to test team"
  teamAddUser testTeam testUser

  expectWSEvent "new test user"
    (isNewUserEvent testUser)

  expectWSEvent "test user joined town square"
    (isPost testUser townSquare "test-user has joined the channel.")

  expectWSEvent "test user joined off-topic"
    (isPost testUser offTopic "test-user has joined the channel.")

  expectWSDone

loginAsNormalUserTest :: TestTree
loginAsNormalUserTest =
    mmTestCase "Logging in with normal account" testConfig $ do
        loginAccount testUserLogin
        Just testUser <- getUserByName (username testUserLogin)

        expectWSEvent "hello" (hasWSEventType WMHello)
        expectWSEvent "status" (isStatusChange testUser "online")
        expectWSDone

initialLoadTest :: TestTree
initialLoadTest =
    mmTestCase "Initial Load" testConfig $ do
        loginAccount testUserLogin

        teams <- getTeams
        print_ (ppShow (fmap teamName teams))

        expectWSEvent "hello" (hasWSEventType WMHello)
        expectWSDone

createChannelTest :: TestTree
createChannelTest =
    mmTestCase "Create Channel" testConfig $ do
        loginAccount testUserLogin

        testUser <- getMe
        teams <- getTeams
        let team Seq.:< _ = Seq.viewl teams

        chan <- createChannel (testMinChannel (teamId team))
        print_ (ppShow chan)

        expectWSEvent "hello" (hasWSEventType WMHello)
        expectWSEvent "test user joins test channel"
          (isPost testUser chan "test-user has joined the channel.")
        expectWSEvent "new channel event" (isChannelCreatedEvent chan)
        expectWSDone

getChannelsTest :: TestTree
getChannelsTest =
    mmTestCase "Get Channels" testConfig $ do
        loginAccount testUserLogin
        teams <- getTeams
        let team Seq.:< _ = Seq.viewl teams
        chans <- getChannels team

        let chan Seq.:< _ = Seq.viewl chans
        print_ (ppShow chan)

        expectWSEvent "hello" (hasWSEventType WMHello)
        expectWSDone

leaveChannelTest :: TestTree
leaveChannelTest =
    mmTestCase "Leave Channel" testConfig $ do
        loginAccount testUserLogin
        Just testUser <- getUserByName (username testUserLogin)
        teams <- getTeams

        let team Seq.:< _ = Seq.viewl teams
        chans <- getChannels team
        print_ (ppShow chans)

        let chan = findChannel chans $ testMinChannelName
        leaveChannel chan

        expectWSEvent "hello" (hasWSEventType WMHello)
        expectWSEvent "leave channel" (isUserLeave testUser chan)
        expectWSDone

joinChannelTest :: TestTree
joinChannelTest =
    mmTestCase "Join Channel" testConfig $ do
        loginAccount testUserLogin
        Just testUser <- getUserByName (username testUserLogin)
        teams <- getTeams

        let team Seq.:< _ = Seq.viewl teams
        chans <- getChannels team
        print_ (ppShow chans)

        let chan = findChannel chans $ testMinChannelName
        joinChannel testUser chan

        members <- getChannelMembers chan
        let expected :: [User]
            expected = [testUser]
        when (fmap userId members /= fmap userId expected) $
            error $ "Expected channel members: " <> show expected <> "\ngot: " <> show members

        expectWSEvent "hello" (hasWSEventType WMHello)
        expectWSEvent "join channel" (isUserJoin testUser chan)
        expectWSEvent "join post"
          (isPost testUser chan "test-user has joined the channel.")
        expectWSEvent "view channel" isViewedChannel
        expectWSDone

deleteChannelTest :: TestTree
deleteChannelTest =
    mmTestCase "Delete Channel" testConfig $ do
        loginAccount testUserLogin
        Just testUser <- getUserByName (username testUserLogin)

        teams <- getTeams
        let team Seq.:< _ = Seq.viewl teams
        chans <- getChannels team

        let toDelete = findChannel chans (testMinChannelName)

        deleteChannel toDelete

        expectWSEvent "hello" (hasWSEventType WMHello)

        expectWSEvent "channel deletion post"
            (isPost testUser toDelete "test-user has archived the channel.")

        expectWSEvent "channel delete event"
            (isChannelDeleteEvent toDelete)

        expectWSDone

clientConfigTest :: TestTree
clientConfigTest =
    mmTestCase "Client Config Test" testConfig $ do
        loginAccount testUserLogin

        print_ "Getting Client Config"
        config <- getClientConfig

        print_ (ppShow config)

        return ()

        -- let Object config = configObj

        -- keyAssert "AboutLink" config
        -- keyAssert "EnableSignInWithEmail" config
        -- keyAssert "RestrictPrivateChannelCreation" config
        -- keyAssert "SiteName" config
        -- keyAssert "TermsOfServiceLink" config
        -- keyAssert "WebsocketSecurePort" config

    -- where
    --     keyAssert k c = unless (HM.member k c) $
    --         let m = T.unpack k <> " key not present"
    --         in print_ m
    --            >> error m
