{-# LANGUAGE OverloadedStrings #-}

module Network.Mattermost.Types where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import           Data.Ratio ( (%) )
import qualified Data.Text as T
import           Data.Time.Clock ( UTCTime )
import           Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import           Network.Connection (ConnectionContext)
import           Network.HTTP.Headers (Header, HeaderName(..), mkHeader)

type Hostname = String
type Port     = Int

-- For now we don't support or expose the ability to reuse connections,
-- but we have this field in case we want to support that in the future.
-- Doing so will require some modifications to withConnection (and uses).
-- Note: don't export this until we support connection reuse.
data AutoClose = No | Yes
  deriving (Read, Show, Eq, Ord)

-- | We return a list of headers so that we can treat
-- the headers like a monoid.
autoCloseToHeader :: AutoClose -> [Header]
autoCloseToHeader No  = []
autoCloseToHeader Yes = [mkHeader HdrConnection "Close"]


data ConnectionData
  = ConnectionData
  { cdHostname      :: Hostname
  , cdPort          :: Port
  , cdAutoClose     :: AutoClose
  , cdConnectionCtx :: ConnectionContext
  , cdToken         :: Maybe Token
  }

mkConnectionData :: Hostname -> Port -> ConnectionContext -> ConnectionData
mkConnectionData host port ctx = ConnectionData
  { cdHostname      = host
  , cdPort          = port
  , cdConnectionCtx = ctx
  , cdAutoClose     = Yes
  , cdToken         = Nothing
  }

data Token = Token String
  deriving (Read, Show, Eq, Ord)

getTokenString :: Token -> String
getTokenString (Token s) = s

--

data Login
  = Login
  { username :: T.Text
  , teamname :: T.Text
  , password :: T.Text
  }

instance A.ToJSON Login where
  toJSON l = A.object ["name"     A..= teamname l
                      ,"login_id" A..= username l
                      ,"password" A..= password l
                      ]


-- | XXX: No idea what this is
data TeamType = O | Unknown
  deriving (Read, Show, Ord, Eq)

instance A.FromJSON TeamType where
  parseJSON (A.String "O") = pure O
  parseJSON _              = pure Unknown

--

newtype Id = Id T.Text
  deriving (Read, Show, Eq, Ord)

instance A.FromJSON Id where
  parseJSON = A.withText "Id" $ \s ->
    pure (Id s)

--

getTeamIdString :: Team -> String
getTeamIdString team = case teamId team of
  Id s -> T.unpack s

data Team
  = Team
  { teamId              :: Id
  , teamCreateAt        :: UTCTime
  , teamUpdateAt        :: UTCTime
  , teamDeleteAt        :: UTCTime
  , teamDisplayName     :: String
  , teamName            :: String
  , teamEmail           :: String
  , teamType            :: TeamType
  , teamCompanyName     :: String
  , teamAllowedDomains  :: String
  , teamInviteId        :: Id
  , teamAllowOpenInvite :: Bool
  }
  deriving (Read, Show, Eq, Ord)

instance A.FromJSON Team where
  parseJSON = A.withObject "Team" $ \v -> Team     <$>
    v A..: "id"                                    <*>
    (millisecondsToUTCTime <$> v A..: "create_at") <*>
    (millisecondsToUTCTime <$> v A..: "update_at") <*>
    (millisecondsToUTCTime <$> v A..: "delete_at") <*>
    v A..: "display_name"                          <*>
    v A..: "name"                                  <*>
    v A..: "email"                                 <*>
    v A..: "type"                                  <*>
    v A..: "company_name"                          <*>
    v A..: "allowed_domains"                       <*>
    v A..: "invite_id"                             <*>
    v A..: "allow_open_invite"

millisecondsToUTCTime :: Integer -> UTCTime
millisecondsToUTCTime ms = posixSecondsToUTCTime (fromRational (ms%1000))

--

-- TODO: It's probably better to return the actual HashMap instead
-- of converting to a list. Let the user of the API decide what
-- they want.
newtype TeamList = TL [Team]
  deriving (Read, Show, Eq, Ord)

instance A.FromJSON TeamList where
  parseJSON = A.withObject "TeamList" $ \hm -> do
    let tl = map snd (HM.toList hm)
    tl' <- mapM A.parseJSON tl
    return (TL tl')
