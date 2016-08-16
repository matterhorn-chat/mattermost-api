{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Mattermost.Types where

import           Data.Hashable ( Hashable )
import qualified Data.Aeson as A
import           Data.Aeson ( (.:) )
import           Data.Aeson.Types ( ToJSONKey
                                  , FromJSONKey
                                  , FromJSON
                                  )
import           Data.HashMap.Strict ( HashMap )
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
data Type = Type T.Text
  deriving (Read, Show, Ord, Eq)

instance A.FromJSON Type where
  parseJSON = A.withText "Type" (pure . Type)

--

-- For converting from type specific Id to generic Id
class IsId x where
  toId   :: x  -> Id
  fromId :: Id -> x

class HasId x where
  getId :: x -> Id

newtype Id = Id { unId :: T.Text }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSONKey, FromJSONKey)

idString :: IsId x => x -> String
idString x = T.unpack (unId i)
  where i = toId x

instance A.FromJSON Id where
  parseJSON = A.withText "Id" (pure . Id)

instance IsId Id where
  toId   = id
  fromId = id

instance HasId Id where
  getId  = id

--

newtype TeamId = TI { unTI :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSONKey, FromJSONKey, FromJSON)

instance IsId TeamId where
  toId   = unTI
  fromId = TI

data Team
  = Team
  { teamId              :: TeamId
  , teamCreateAt        :: UTCTime
  , teamUpdateAt        :: UTCTime
  , teamDeleteAt        :: UTCTime
  , teamDisplayName     :: String
  , teamName            :: String
  , teamEmail           :: String
  , teamType            :: Type
  , teamCompanyName     :: String
  , teamAllowedDomains  :: String
  , teamInviteId        :: Id -- XXX: What type of Id is this?
  , teamAllowOpenInvite :: Bool
  }
  deriving (Read, Show, Eq, Ord)

instance HasId Team where
  getId = toId . teamId

instance A.FromJSON Team where
  parseJSON = A.withObject "Team" $ \v -> do
    teamId              <- v .: "id"
    teamCreateAt        <- millisecondsToUTCTime <$> v .: "create_at"
    teamUpdateAt        <- millisecondsToUTCTime <$> v .: "update_at"
    teamDeleteAt        <- millisecondsToUTCTime <$> v .: "delete_at"
    teamDisplayName     <- v .: "display_name"
    teamName            <- v .: "name"
    teamEmail           <- v .: "email"
    teamType            <- v .: "type"
    teamCompanyName     <- v .: "company_name"
    teamAllowedDomains  <- v .: "allowed_domains"
    teamInviteId        <- v .: "invite_id"
    teamAllowOpenInvite <- v .: "allow_open_invite"
    return Team { .. }

--

newtype ChannelId = CI { unCI :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSONKey, FromJSONKey, FromJSON)

instance IsId ChannelId where
  toId   = unCI
  fromId = CI

data Channel
  = Channel
  { channelId            :: ChannelId
  , channelCreateAt      :: UTCTime
  , channelUpdateAt      :: UTCTime
  , channelDeleteAt      :: UTCTime
  , channelTeamId        :: TeamId
  , channelType          :: Type
  , channelDisplayName   :: String
  , channelName          :: String
  , channelHeader        :: String
  , channelPurpose       :: String
  , channelLastPostAt    :: UTCTime
  , channelTotalMsgCount :: Int
  , channelExtraUpdateAt :: UTCTime
  , channelCreatorId     :: UserId
  } deriving (Read, Show, Eq, Ord)

instance HasId Channel where
  getId = toId . channelId

instance A.FromJSON Channel where
  parseJSON = A.withObject "Channel" $ \v -> do
    channelId              <- v .: "id"
    channelCreateAt        <- millisecondsToUTCTime <$> v .: "create_at"
    channelUpdateAt        <- millisecondsToUTCTime <$> v .: "update_at"
    channelDeleteAt        <- millisecondsToUTCTime <$> v .: "delete_at"
    channelTeamId          <- v .: "team_id"
    channelType            <- v .: "type"
    channelDisplayName     <- v .: "display_name"
    channelName            <- v .: "name"
    channelHeader          <- v .: "header"
    channelPurpose         <- v .: "purpose"
    channelLastPostAt      <- millisecondsToUTCTime <$> v .: "last_post_at"
    channelTotalMsgCount   <- v .: "total_msg_count"
    channelExtraUpdateAt   <- millisecondsToUTCTime <$> v .: "extra_update_at"
    channelCreatorId       <- v .: "creator_id"
    return Channel { .. }

instance HasId ChannelData where
  getId = toId . channelDataChannelId

data ChannelData
  = ChannelData
  { channelDataChannelId    :: ChannelId
  , channelDataUserId       :: UserId
  , channelDataRoles        :: String -- XXX: what goes here
  , channelDataLastViewedAt :: UTCTime
  , channelDataMsgCount     :: Int
  , channelDataMentionCount :: Int
  , channelDataNotifyProps  :: HashMap String String
  , channelDataLastUpdateAt :: UTCTime
  } deriving (Read, Show, Eq)

instance A.FromJSON ChannelData where
  parseJSON = A.withObject "ChanelData" $ \o -> do
    channelDataChannelId <- o .: "channel_id"
    channelDataUserId    <- o .: "user_id"
    channelDataRoles     <- o .: "roles"
    channelDataLastViewedAt <- millisecondsToUTCTime <$> o .: "last_viewed_at"
    channelDataMsgCount     <- o .: "msg_count"
    channelDataMentionCount <- o .: "mention_count"
    channelDataNotifyProps  <- o .: "notify_props"
    channelDataLastUpdateAt <- millisecondsToUTCTime <$> o .: "last_update_at"
    return ChannelData { .. }

-- For reasons I cannot fathom, MM returns two thing here.
-- First they return the channels plus a lot of meta data about each
-- one. Second they give you a "members" field that has
-- additional data about the channels, none of which appears
-- to be data about the members.
--
-- We refer to the extra meta data as ChannelData.
data Channels = Channels [Channel] (HashMap ChannelId ChannelData)
  deriving (Read, Show, Eq)

instance A.FromJSON Channels where
  parseJSON = A.withObject "Channels" $ \o -> do
    channels <- o .: "channels"
    chandata <- o .: "members"
    return $ Channels channels chandata

--

newtype UserId = UI { unUI :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSONKey, FromJSONKey, FromJSON)

instance IsId UserId where
  toId   = unUI
  fromId = UI

data UserProfile
  = UserProfile
  { userProfileEmail          :: String
  , userProfileRoles          :: String
  , userProfileLastActivityAt :: UTCTime
  , userProfileFirstName      :: String
  , userProfileAuthService    :: String
  , userProfileLocale         :: String
  , userProfileUsername       :: String
  , userProfileAuthData       :: String
  , userProfileLastName       :: String
  , userProfileId             :: UserId
  , userProfileNickname       :: String
  , userProfileDeleteAt       :: UTCTime
  , userProfileCreateAt       :: UTCTime
  } deriving (Read, Show, Eq, Ord)

instance HasId UserProfile where
  getId = toId . userProfileId

instance A.FromJSON UserProfile where
  parseJSON = A.withObject "UserProfile" $ \v -> do
    userProfileEmail          <- v .: "email"
    userProfileRoles          <- v .: "roles"
    userProfileLastActivityAt <- millisecondsToUTCTime <$> v .: "last_activity_at"
    userProfileFirstName      <- v .: "first_name"
    userProfileAuthService    <- v .: "auth_service"
    userProfileLocale         <- v .: "locale"
    userProfileUsername       <- v .: "username"
    userProfileAuthData       <- v .: "auth_data"
    userProfileLastName       <- v .: "last_name"
    userProfileId             <- v .: "id"
    userProfileNickname       <- v .: "nickname"
    userProfileDeleteAt       <- millisecondsToUTCTime <$> v .: "delete_at"
    userProfileCreateAt       <- millisecondsToUTCTime <$> v .: "create_at"
    return UserProfile { .. }

--

instance HasId User where
  getId = toId . userId

data User
  = User
  { userId                 :: UserId
  , userCreateAt           :: UTCTime
  , userUpdateAt           :: UTCTime
  , userDeleteAt           :: UTCTime
  , userUsername           :: String
  , userAuthData           :: String
  , userAuthService        :: String
  , userEmail              :: String
  , userEmailVerified      :: Bool
  , userNickname           :: String
  , userFirstName          :: String
  , userLastName           :: String
  , userRoles              :: String -- XXX: what are the options?
  , userLastActivityAt     :: UTCTime
  , userLastPingAt         :: UTCTime
  , userNotifyProps        :: HashMap String String -- See NotifyProps type below
  , userLastPasswordUpdate :: UTCTime
  , userLastPictureUpdate  :: UTCTime
  , userLocale             :: String
  } deriving (Read, Show, Eq)

instance A.FromJSON User where
  parseJSON = A.withObject "User" $ \o -> do
    userId                 <- o .: "id"
    userCreateAt           <- millisecondsToUTCTime <$> o .: "create_at"
    userUpdateAt           <- millisecondsToUTCTime <$> o .: "update_at"
    userDeleteAt           <- millisecondsToUTCTime <$> o .: "delete_at"
    userUsername           <- o .: "username"
    userAuthData           <- o .: "auth_data"
    userAuthService        <- o .: "auth_service"
    userEmail              <- o .: "email"
    userEmailVerified      <- o .: "email_verified"
    userNickname           <- o .: "nickname"
    userFirstName          <- o .: "first_name"
    userLastName           <- o .: "last_name"
    userRoles              <- o .: "roles"
    userLastActivityAt     <- millisecondsToUTCTime <$> o .: "last_activity_at"
    userLastPingAt         <- millisecondsToUTCTime <$> o .: "last_ping_at"
    userNotifyProps        <- o .: "notify_props"
    userLastPasswordUpdate <- millisecondsToUTCTime <$> o .: "last_password_update"
    userLastPictureUpdate  <- millisecondsToUTCTime <$> o .: "last_picture_update"
    userLocale             <- o .: "locale"
    return User { .. }

-- XXX: Let's defer making a custom type for this until
-- we have more information about how we'll use it.
-- -- XXX: A bunch of these should be bools, but aeson is not
-- -- parsing them as such. So for now I'm just setting them as strings.
-- data NotifyProps
--   = NotifyProps
--   { notifyPropsAll          :: String -- bool
--   , notifyPropsChannel      :: String -- bool
--   , notifyPropsDesktop      :: String -- XXX: what goes here
--   , notifyPropsDesktopSound :: String -- bool
--   , notifyPropsEmail        :: String -- bool
--   , notifyPropsFirstName    :: String -- bool
--   , notifyPropsMentionKeys  :: [String]
--   , notifyPropsPush         :: String -- XXX: what goes here
--   } deriving (Read, Show, Eq, Ord)
--
-- instance A.FromJSON NotifyProps where
--   parseJSON = A.withObject "NotifyProps" $ \o -> do
--     notifyPropsAll          <- o .: "all"
--     notifyPropsChannel      <- o .: "channel"
--     notifyPropsDesktop      <- o .: "desktop"
--     notifyPropsDesktopSound <- o .: "desktop_sound"
--     notifyPropsEmail        <- o .: "email"
--     notifyPropsFirstName    <- o .: "first_name"
--     mentionKeys             <- T.splitOn "," <$> o .: "mention_keys"
--     let notifyPropsMentionKeys = map T.unpack mentionKeys
--     notifyPropsPush         <- o .: "push"
--     return NotifyProps { .. }

--

newtype PostId = PI { unPI :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSONKey, FromJSONKey, FromJSON)

instance IsId PostId where
  toId   = unPI
  fromId = PI

data Post
  = Post
  { postPendingPostId :: PostId
  , postOriginalId    :: PostId
  , postProps         :: A.Value
  , postRootId        :: String
  , postFilenames     :: A.Value
  , postId            :: PostId
  , postType          :: Type
  , postMessage       :: String
  , postDeleteAt      :: UTCTime
  , postHashtags      :: String
  , postUpdateAt      :: UTCTime
  , postUserId        :: UserId
  , postCreateAt      :: UTCTime
  , postParentId      :: PostId
  , postChannelId     :: ChannelId
  } deriving (Read, Show, Eq)

instance HasId Post where
  getId = toId . postId

instance A.FromJSON Post where
  parseJSON = A.withObject "Post" $ \v -> do
    postPendingPostId <- v .: "pending_post_id"
    postOriginalId    <- v .: "original_id"
    postProps         <- v .: "props"
    postRootId        <- v .: "root_id"
    postFilenames     <- v .: "filenames"
    postId            <- v .: "id"
    postType          <- v .: "type"
    postMessage       <- v .: "message"
    postDeleteAt      <- millisecondsToUTCTime <$> v .: "delete_at"
    postHashtags      <- v .: "hashtags"
    postUpdateAt      <- millisecondsToUTCTime <$> v .: "update_at"
    postUserId        <- v .: "user_id"
    postCreateAt      <- millisecondsToUTCTime <$> v .: "create_at"
    postParentId      <- v .: "parent_id"
    postChannelId     <- v .: "channel_id"
    return Post { .. }

--

data Posts
  = Posts
  { postsPosts :: HM.HashMap PostId Post
  , postsOrder :: [PostId]
  } deriving (Read, Show, Eq)

instance A.FromJSON Posts where
  parseJSON = A.withObject "Posts" $ \v -> do
    postsPosts <- v .: "posts"
    postsOrder <- v .: "order"
    return Posts { .. }

--

millisecondsToUTCTime :: Integer -> UTCTime
millisecondsToUTCTime ms = posixSecondsToUTCTime (fromRational (ms%1000))

--

