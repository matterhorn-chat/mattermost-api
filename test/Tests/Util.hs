module Tests.Util
  ( reportJSONExceptions
  , adminAccount
  , whenDebug
  , createAdminAccount
  , loginAccount
  , loginAdminAccount
  , createAccount
  )
where

import Control.Monad (void, when, join)
import qualified Control.Exception as E
import Data.Monoid ((<>))
import qualified Data.Text as T

import Network.Mattermost
import Network.Mattermost.Exceptions

import Tests.Types

debug :: Bool
debug = False

whenDebug :: IO () -> IO ()
whenDebug = when debug

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

createAdminAccount :: Config -> ConnectionData -> (String -> IO ()) -> IO ()
createAdminAccount cfg cd prnt = do
  void $ mmUsersCreate cd $ adminAccount cfg
  whenDebug $ prnt "Admin Account created"

loginAccount :: ConnectionData -> Login -> (String -> IO ()) -> IO Token
loginAccount cd login prnt = do
  (token, _mmUser) <- join (hoistE <$> mmLogin cd login)
  whenDebug $ prnt $ "Authenticated as " ++ T.unpack (username login)
  return token

loginAdminAccount :: Config -> ConnectionData -> (String -> IO ()) -> IO Token
loginAdminAccount cfg cd = loginAccount cd admin
  where
  admin = Login { username = configUsername cfg
                , password = configPassword cfg
                }

createAccount :: ConnectionData -> Token -> UsersCreate -> (String -> IO ()) -> IO User
createAccount cd token account prnt = do
  newUser <- mmUsersCreateWithToken cd token account
  whenDebug $
    prnt $ "account created for " <> (T.unpack $ usersCreateUsername account)
  return newUser
