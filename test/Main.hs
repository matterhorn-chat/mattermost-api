{-# LANGUAGE OverloadedStrings #-}
module Main (
  main
) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Control.Monad ( join )

import           Text.Show.Pretty ( ppShow )

import           Data.Aeson
import qualified Data.HashMap.Strict as HM

import           Test.Tasty
import           Test.Tasty.HUnit

import           Network.Mattermost
import           Network.Mattermost.Logging
import           Network.Mattermost.Util

main :: IO ()
main = defaultMain tests

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

-- Test groups

tests :: TestTree
tests = testGroup "Tests" [setup,unitTests]

unitTests :: TestTree
unitTests = testGroup "Units" [createNormalUserTest
                              ,loginAsNormalUserTest
                              ]

-- Test definitions

setup :: TestTree
setup = testCaseSteps "Setup" $ \prnt -> do
  prnt "Creating connection"
  cd <- initConnectionDataInsecure (T.unpack (configHostname testConfig))
                                   (fromIntegral (configPort testConfig))
  -- XXX: Use something like this if you want logging (useful when debugging)
  -- let cd = cd' `withLogger` mmLoggerDebugErr

  prnt "Creating Admin account"
  adminUser <- createAdminAccount cd prnt
  prnt "Logging into Admin account"
  adminToken <- loginAdminAccount cd prnt

  prnt "Creating test team"
  _testTeam <- createTestTeam cd adminToken prnt

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

createNormalUserTest :: TestTree
createNormalUserTest = testCaseSteps "Creating normal account" $ \prnt -> do
  cd <- initConnectionDataInsecure (T.unpack (configHostname testConfig))
                                   (fromIntegral (configPort testConfig))
  prnt "Logging into Admin account"
  adminToken <- loginAdminAccount cd prnt
  prnt "Creating test account"
  testUser   <- createTestAccount cd adminToken prnt
  return ()

loginAsNormalUserTest :: TestTree
loginAsNormalUserTest = testCaseSteps "Logging to normal account" $ \prnt -> do
  cd <- initConnectionDataInsecure (T.unpack (configHostname testConfig))
                                   (fromIntegral (configPort testConfig))
  userToken <- loginAccount cd testUserLogin prnt
  return ()

-- Wrapper functions used in test cases

createAdminAccount :: ConnectionData -> (String -> IO ()) -> IO ()
createAdminAccount cd prnt = do
  let newAccount = UsersCreate { usersCreateEmail          = configEmail    testConfig
                               , usersCreatePassword       = configPassword testConfig
                               , usersCreateUsername       = configUsername testConfig
                               , usersCreateAllowMarketing = True
                               }
  newUser <- mmUsersCreate cd newAccount
  prnt "Admin Account created"

createTestTeam :: ConnectionData -> Token -> (String -> IO ()) -> IO Team
createTestTeam cd token prnt = do
  let newTeam = TeamsCreate { teamsCreateDisplayName = "Test Team"
                            , teamsCreateName        = "testteam"
                            , teamsCreateType        = Ordinary
                            }
  team <- mmCreateTeam cd token newTeam
  prnt "Test team created"
  return team

createTestAccount :: ConnectionData -> Token -> (String -> IO ()) -> IO User
createTestAccount cd token prnt = do
  let newAccount = UsersCreate { usersCreateEmail          = "test-user@example.com"
                               , usersCreatePassword       = password testUserLogin
                               , usersCreateUsername       = username testUserLogin
                               , usersCreateAllowMarketing = False
                               }
  newUser <- mmUsersCreateWithToken cd token newAccount
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
  (token, mmUser) <- join (hoistE <$> mmLogin cd login)
  prnt $ "Authenticated as " ++ T.unpack (username login)
  return token
