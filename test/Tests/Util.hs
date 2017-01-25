module Tests.Util
  ( reportJSONExceptions
  , adminAccount
  )
where

import qualified Control.Exception as E

import Network.Mattermost
import Network.Mattermost.Exceptions

import Tests.Types

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
