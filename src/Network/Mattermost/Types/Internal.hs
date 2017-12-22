-- | The types defined in this module are exported to facilitate
-- efforts such as QuickCheck and other instrospection efforts, but
-- users are advised to avoid using these types wherever possible:
-- they can be used in a manner that would cause significant
-- disruption and may be subject to change without being reflected in
-- the mattermost-api version.

module Network.Mattermost.Types.Internal where

import Data.Pool (Pool)
import Network.Connection (ConnectionContext, Connection)
import Network.HTTP.Headers (Header, HeaderName(..), mkHeader)
import Network.Mattermost.Types.Base

data Token = Token String
  deriving (Read, Show, Eq, Ord)

getTokenString :: Token -> String
getTokenString (Token s) = s

data AutoClose = No | Yes
  deriving (Read, Show, Eq, Ord)

-- | We return a list of headers so that we can treat
-- the headers like a monoid.
autoCloseToHeader :: AutoClose -> [Header]
autoCloseToHeader No  = []
autoCloseToHeader Yes = [mkHeader HdrConnection "Close"]


data ConnectionData
  = ConnectionData
  { cdHostname       :: Hostname
  , cdPort           :: Port
  , cdAutoClose      :: AutoClose
  , cdConnectionPool :: Pool Connection
  , cdConnectionCtx  :: ConnectionContext
  , cdToken          :: Maybe Token
  , cdLogger         :: Maybe Logger
  , cdUseTLS         :: Bool
  }
