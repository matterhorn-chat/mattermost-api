{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Mattermost.Types where

import           Control.Applicative
import           Text.Printf ( printf )
import           Data.Hashable ( Hashable )
import qualified Data.Aeson as A
import           Data.Aeson ( (.:), (.=), (.:?) )
import           Data.Aeson.Types ( ToJSONKey
                                  , FromJSONKey
                                  , FromJSON
                                  , ToJSON
                                  , Parser
                                  )
import           Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import           Data.Ratio ( (%) )
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock ( UTCTime, getCurrentTime )
import           Data.Time.Clock.POSIX ( posixSecondsToUTCTime
                                       , utcTimeToPOSIXSeconds )
import           Network.Connection (ConnectionContext, initConnectionContext)
import           Network.HTTP.Base (RequestMethod)
import           Network.HTTP.Headers (Header, HeaderName(..), mkHeader)

-- | A 'Logger' is any function which responds to log events:
type Logger = LogEvent -> IO ()

-- | If there is a 'Logger' in the 'ConnectionData' struct, it will
--   be sporadically called with values of type 'LogEvent'.
data LogEvent = LogEvent
  { logFunction  :: String
  , logEventType :: LogEventType
  } deriving (Eq, Show)

-- | A 'LogEventType' describes the particular event that happened
data LogEventType
  = HttpRequest RequestMethod String (Maybe A.Value)
  | HttpResponse Int String (Maybe A.Value)
  | WebSocketRequest A.Value
  | WebSocketResponse A.Value
  | WebSocketPing
  | WebSocketPong
    deriving (Eq, Show)

runLogger :: ConnectionData -> String -> LogEventType -> IO ()
runLogger ConnectionData { cdLogger = Just l } n ev =
  l (LogEvent n ev)
runLogger _ _ _ = return ()

maybeFail :: Parser a -> Parser (Maybe a)
maybeFail p = (Just <$> p) <|> (return Nothing)

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
  , cdLogger        :: Maybe Logger
  }

mkConnectionData :: Hostname -> Port -> ConnectionContext -> ConnectionData
mkConnectionData host port ctx = ConnectionData
  { cdHostname      = host
  , cdPort          = port
  , cdConnectionCtx = ctx
  , cdAutoClose     = Yes
  , cdToken         = Nothing
  , cdLogger        = Nothing
  }

initConnectionData :: Hostname -> Port -> IO ConnectionData
initConnectionData host port = do
  ctx <- initConnectionContext
  return (mkConnectionData host port ctx)

withLogger :: ConnectionData -> Logger -> ConnectionData
withLogger cd logger = cd { cdLogger = Just logger }

noLogger :: ConnectionData -> ConnectionData
noLogger cd = cd { cdLogger = Nothing }

data Token = Token String
  deriving (Read, Show, Eq, Ord)

getTokenString :: Token -> String
getTokenString (Token s) = s

--

data Login
  = Login
  { username :: Text
  , password :: Text
  }

instance A.ToJSON Login where
  toJSON l = A.object ["login_id" A..= username l
                      ,"password" A..= password l
                      -- XXX do we also need "token" -> "" like the web client?
                      ]


data SetChannelHeader = SetChannelHeader
  { setChannelHeaderChanId :: ChannelId
  , setChannelHeaderString :: Text
  }

instance A.ToJSON SetChannelHeader where
  toJSON (SetChannelHeader cId p) =
      A.object ["channel_id" A..= cId
               ,"channel_header" A..= p
               ]

data Type = Ordinary
          | Direct
          | Private
          | Unknown Text
  deriving (Read, Show, Ord, Eq)

instance A.FromJSON Type where
  parseJSON = A.withText "Type" $ \t ->
      return $ if | t == "O"  -> Ordinary
                  | t == "D"  -> Direct
                  | t == "P"  -> Private
                  | otherwise -> Unknown t

instance A.ToJSON Type where
  toJSON Direct    = A.toJSON ("D"::Text)
  toJSON Ordinary  = A.toJSON ("O"::Text)
  toJSON Private   = A.toJSON ("P"::Text)
  toJSON (Unknown t) = A.toJSON t

--

-- For converting from type specific Id to generic Id
class IsId x where
  toId   :: x  -> Id
  fromId :: Id -> x

class HasId x y | x -> y where
  getId :: x -> y

newtype Id = Id { unId :: Text }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSONKey)

idString :: IsId x => x -> Text
idString x = unId i
  where i = toId x

instance A.FromJSON Id where
  parseJSON = A.withText "Id" $ \t ->
      case T.null t of
          False -> return $ Id t
          True -> fail "Empty ID"

instance IsId Id where
  toId   = id
  fromId = id

instance HasId Id Id where
  getId  = id

--

newtype TeamId = TI { unTI :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

instance IsId TeamId where
  toId   = unTI
  fromId = TI

data Team
  = Team
  { teamId              :: TeamId
  , teamCreateAt        :: UTCTime
  , teamUpdateAt        :: UTCTime
  , teamDeleteAt        :: UTCTime
  , teamDisplayName     :: Text
  , teamName            :: Text
  , teamEmail           :: Text
  , teamType            :: Type
  , teamCompanyName     :: Text
  , teamAllowedDomains  :: Text
  , teamInviteId        :: Id -- XXX: What type of Id is this?
  , teamAllowOpenInvite :: Bool
  }
  deriving (Read, Show, Eq, Ord)

instance HasId Team TeamId where
  getId = teamId

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
  deriving (Read, Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

instance IsId ChannelId where
  toId   = unCI
  fromId = CI

data Channel
  = Channel
  { channelId            :: ChannelId
  , channelCreateAt      :: UTCTime
  , channelUpdateAt      :: UTCTime
  , channelDeleteAt      :: UTCTime
  , channelTeamId        :: Maybe TeamId
  , channelType          :: Type
  , channelDisplayName   :: Text
  , channelName          :: Text
  , channelHeader        :: Text
  , channelPurpose       :: Text
  , channelLastPostAt    :: UTCTime
  , channelTotalMsgCount :: Int
  , channelExtraUpdateAt :: UTCTime
  , channelCreatorId     :: Maybe UserId
  } deriving (Read, Show, Eq, Ord)

instance HasId Channel ChannelId where
  getId = channelId

instance A.FromJSON Channel where
  parseJSON = A.withObject "Channel" $ \v -> do
    channelId              <- v .: "id"
    channelCreateAt        <- millisecondsToUTCTime <$> v .: "create_at"
    channelUpdateAt        <- millisecondsToUTCTime <$> v .: "update_at"
    channelDeleteAt        <- millisecondsToUTCTime <$> v .: "delete_at"
    channelTeamId          <- maybeFail (v .: "team_id")
    channelType            <- v .: "type"
    channelDisplayName     <- v .: "display_name"
    channelName            <- v .: "name"
    channelHeader          <- v .: "header"
    channelPurpose         <- v .: "purpose"
    channelLastPostAt      <- millisecondsToUTCTime <$> v .: "last_post_at"
    channelTotalMsgCount   <- v .: "total_msg_count"
    channelExtraUpdateAt   <- millisecondsToUTCTime <$> v .: "extra_update_at"
    channelCreatorId       <- maybeFail (v .: "creator_id")
    return Channel { .. }

-- This type only exists so that we can strip off the
-- outer most layer in mmGetChannel. See the
-- FromJSON instance.
newtype SingleChannel = SC Channel
  deriving (Read, Show, Eq, Ord)

instance A.FromJSON SingleChannel where
  parseJSON = A.withObject "SingleChannel" $ \v -> do
    channel <- v .: "channel"
    return (SC channel)

instance HasId ChannelData ChannelId where
  getId = channelDataChannelId

data ChannelData
  = ChannelData
  { channelDataChannelId    :: ChannelId
  , channelDataUserId       :: UserId
  , channelDataRoles        :: Text -- XXX: what goes here
  , channelDataLastViewedAt :: UTCTime
  , channelDataMsgCount     :: Int
  , channelDataMentionCount :: Int
  , channelDataNotifyProps  :: HashMap Text Text
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
data Channels = Channels (Seq Channel) (HashMap ChannelId ChannelData)
  deriving (Read, Show, Eq)

instance A.FromJSON Channels where
  parseJSON = A.withObject "Channels" $ \o -> do
    channels <- o .: "channels"
    chandata <- o .: "members"
    return $ Channels channels chandata

data MoreChannels = MoreChannels (Seq Channel)
  deriving (Read, Show, Eq)

instance A.FromJSON MoreChannels where
  parseJSON = A.withObject "MoreChannels" $ \o -> do
    channels <- o .: "channels"
    return $ MoreChannels channels

data MinChannel = MinChannel
  { minChannelName        :: Text
  , minChannelDisplayName :: Text
  , minChannelPurpose     :: Maybe Text
  , minChannelHeader      :: Maybe Text
  , minChannelType        :: Type
  } deriving (Read, Eq, Show)

instance A.ToJSON MinChannel where
  toJSON MinChannel { .. }  = A.object $
    [ "name"         .= minChannelName
    , "display_name" .= minChannelDisplayName
    , "type"         .= minChannelType
    ] ++
    [ "purpose" .= p | Just p <- [minChannelPurpose] ] ++
    [ "header"  .= h | Just h <- [minChannelHeader] ]
--

newtype UserId = UI { unUI :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

instance IsId UserId where
  toId   = unUI
  fromId = UI

data UserProfile
  = UserProfile
  { userProfileEmail          :: Text
  , userProfileRoles          :: Text
  , userProfileFirstName      :: Text
  , userProfileAuthService    :: Text
  , userProfileLocale         :: Text
  , userProfileUsername       :: Text
  , userProfileAuthData       :: Text
  , userProfileLastName       :: Text
  , userProfileId             :: UserId
  , userProfileNickname       :: Text
  , userProfileDeleteAt       :: UTCTime
  , userProfileCreateAt       :: UTCTime
  } deriving (Read, Show, Eq, Ord)

instance HasId UserProfile UserId where
  getId = userProfileId

instance A.FromJSON UserProfile where
  parseJSON = A.withObject "UserProfile" $ \v -> do
    userProfileEmail          <- v .: "email"
    userProfileRoles          <- v .: "roles"
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

-- Note: there's lots of other stuff in an initial_load response but
-- this is what we use for now.
data InitialLoad
  = InitialLoad
  { initialLoadUser :: User
  , initialLoadTeams :: Seq Team
  } deriving (Eq, Show)

instance A.FromJSON InitialLoad where
  parseJSON = A.withObject "InitialLoad" $ \o -> do
    initialLoadUser        <- o .: "user"
    initialLoadTeams       <- o .: "teams"
    return InitialLoad { .. }

--

instance HasId User UserId where
  getId = userId

data User
  = User
  { userId                 :: UserId
  , userCreateAt           :: UTCTime
  , userUpdateAt           :: UTCTime
  , userDeleteAt           :: UTCTime
  , userUsername           :: Text
  , userAuthData           :: Text
  , userAuthService        :: Text
  , userEmail              :: Text
  , userEmailVerified      :: Bool
  , userNickname           :: Text
  , userFirstName          :: Text
  , userLastName           :: Text
  , userRoles              :: Text -- XXX: what are the options?
  , userNotifyProps        :: HashMap Text Text -- See NotifyProps type below
  , userLastPasswordUpdate :: UTCTime
  , userLastPictureUpdate  :: Maybe UTCTime
  , userLocale             :: Text
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
    userNotifyProps        <- o .: "notify_props"
    userLastPasswordUpdate <- millisecondsToUTCTime <$> o .: "last_password_update"
    userLastPictureUpdate  <- (millisecondsToUTCTime <$>) <$> (o .:? "last_picture_update")
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

data PostProps
  = PostProps
  { postPropsOverrideIconUrl  :: Maybe Text
  , postPropsOverrideUsername :: Maybe Text
  , postPropsAttachments      :: Maybe A.Value
  } deriving (Read, Show, Eq)

instance A.FromJSON PostProps where
  parseJSON = A.withObject "Props" $ \v -> do
    postPropsOverrideIconUrl  <- v .:? "override_icon_url"
    postPropsOverrideUsername <- v .:? "override_username"
    postPropsAttachments      <- v .:? "attachments"
    return PostProps { .. }

instance A.ToJSON PostProps where
  toJSON PostProps { .. } = A.object $
    [ "override_icon_url" .= v | Just v <- [postPropsOverrideIconUrl] ] ++
    [ "override_username" .= v | Just v <- [postPropsOverrideUsername] ] ++
    [ "attachments" .= v | Just v <- [postPropsAttachments] ]

newtype PostId = PI { unPI :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

instance IsId PostId where
  toId   = unPI
  fromId = PI

data Post
  = Post
  { postPendingPostId :: Maybe PostId
  , postOriginalId    :: Maybe PostId
  , postProps         :: PostProps
  , postRootId        :: Text
  , postFilenames     :: Seq Text
  , postId            :: PostId
  , postType          :: Type
  , postMessage       :: Text
  , postDeleteAt      :: UTCTime
  , postHashtags      :: Text
  , postUpdateAt      :: UTCTime
  , postUserId        :: Maybe UserId
  , postCreateAt      :: UTCTime
  , postParentId      :: Maybe PostId
  , postChannelId     :: ChannelId
  } deriving (Read, Show, Eq)

instance HasId Post PostId where
  getId = postId

instance A.FromJSON Post where
  parseJSON = A.withObject "Post" $ \v -> do
    postPendingPostId <- maybeFail (v .: "pending_post_id")
    postOriginalId    <- maybeFail (v .: "original_id")
    postProps         <- v .: "props"
    postRootId        <- v .: "root_id"
    postFilenames     <- v .: "filenames"
    postId            <- v .: "id"
    postType          <- v .: "type"
    postMessage       <- v .: "message"
    postDeleteAt      <- millisecondsToUTCTime <$> v .: "delete_at"
    postHashtags      <- v .: "hashtags"
    postUpdateAt      <- millisecondsToUTCTime <$> v .: "update_at"
    postUserId        <- maybeFail (v .: "user_id")
    postCreateAt      <- millisecondsToUTCTime <$> v .: "create_at"
    postParentId      <- maybeFail (v .: "parent_id")
    postChannelId     <- v .: "channel_id"
    return Post { .. }

instance A.ToJSON Post where
  toJSON Post { .. } = A.object
    [ "pending_post_id" .= postPendingPostId
    , "original_id"     .= postOriginalId
    , "props"           .= postProps
    , "root_id"         .= postRootId
    , "filenames"       .= postFilenames
    , "id"              .= postId
    , "type"            .= postType
    , "message"         .= postMessage
    , "delete_at"       .= postDeleteAt
    , "hashtags"        .= postHashtags
    , "update_at"       .= postUpdateAt
    , "user_id"         .= postUserId
    , "create_at"       .= postCreateAt
    , "parent_id"       .= postParentId
    , "channel_id"      .= postChannelId
    ]

data PendingPost
  = PendingPost
  { pendingPostChannelId :: ChannelId
  , pendingPostCreateAt  :: UTCTime
  , pendingPostFilenames :: Seq FilePath
  , pendingPostMessage   :: Text
  , pendingPostId        :: PendingPostId
  , pendingPostUserId    :: UserId
  } deriving (Read, Show, Eq)

instance A.ToJSON PendingPost where
  toJSON post = A.object
    [ "channel_id"      .= pendingPostChannelId post
    , "create_at"       .= utcTimeToMilliseconds (pendingPostCreateAt  post)
    , "filenames"       .= pendingPostFilenames post
    , "message"         .= pendingPostMessage   post
    , "pending_post_id" .= pendingPostId        post
    , "user_id"         .= pendingPostUserId    post
    ]

newtype PendingPostId = PPI { unPPI :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

instance IsId PendingPostId where
  toId   = unPPI
  fromId = PPI

instance HasId PendingPost PendingPostId where
  getId = pendingPostId

mkPendingPost :: Text -> UserId -> ChannelId -> IO PendingPost
mkPendingPost msg userid channelid = do
  now <- getCurrentTime
  let ms  = utcTimeToMilliseconds now :: Int
      pid = T.pack $ printf "%s:%d" (idString userid) ms
  return PendingPost
    { pendingPostId        = PPI (Id pid)
    , pendingPostChannelId = channelid
    , pendingPostCreateAt  = now
    , pendingPostFilenames = S.empty
    , pendingPostMessage   = msg
    , pendingPostUserId    = userid
    }

--

data Posts
  = Posts
  { postsPosts :: HM.HashMap PostId Post
  , postsOrder :: Seq PostId
  } deriving (Read, Show, Eq)

instance A.FromJSON Posts where
  parseJSON = A.withObject "Posts" $ \v -> do
    postsPosts <- v .: "posts"
    postsOrder <- v .: "order"
    return Posts { .. }

--

millisecondsToUTCTime :: Integer -> UTCTime
millisecondsToUTCTime ms = posixSecondsToUTCTime (fromRational (ms%1000))

utcTimeToMilliseconds :: UTCTime -> Int
utcTimeToMilliseconds utc = truncate ((utcTimeToPOSIXSeconds utc)*1000)

--

data MinCommand
  = MinCommand
  { minComChannelId :: ChannelId
  , minComCommand   :: Text
  , minComSuggest   :: Bool -- XXX: really?
  } deriving (Read, Show, Eq)

instance A.ToJSON MinCommand where
  toJSON MinCommand { .. } = A.object
    [ "channelId" .= minComChannelId
    , "command"   .= minComCommand
    , "suggest"   .=
      if minComSuggest
        then ("true" :: Text)
        else ("false" :: Text)
    ]

--

data Command
  = Command
  { commandId               :: CommandId
  , commandToken            :: Token
  , commandCreateAt         :: UTCTime
  , commandUpdateAt         :: UTCTime
  , commandDeleteAt         :: UTCTime
  , commandCreatorId        :: UserId
  , commandTeamId           :: TeamId
  , commandTrigger          :: Text
  , commandMethod           :: Text
  , commandUsername         :: Text
  , commandIconURL          :: Text
  , commandAutoComplete     :: Bool
  , commandAutoCompleteDesc :: Text
  , commandAutoCompleteHint :: Text
  , commandDisplayName      :: Text
  , commandDescription      :: Text
  , commandURL              :: Text
  } deriving (Read, Show, Eq)

newtype CommandId = CmdI { unCmdI :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

instance IsId CommandId where
  toId   = unCmdI
  fromId = CmdI

instance HasId Command CommandId where
  getId = commandId
