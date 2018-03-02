{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Mattermost.Types
    ( module Network.Mattermost.Types
    , module Network.Mattermost.Types.Base
    )
    where

import           Control.Applicative
import           Text.Printf ( PrintfArg(..), printf )
import           Data.Hashable ( Hashable )
import qualified Data.Aeson as A
import           Data.Aeson ( (.:), (.=), (.:?), (.!=) )
import           Data.Aeson.Types ( ToJSONKey
                                  , FromJSONKey
                                  , FromJSON
                                  , ToJSON
                                  , Parser
                                  , typeMismatch
                                  )
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ( (<>) )
import qualified Data.Pool as Pool
import           Data.Ratio ( (%) )
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.Time (NominalDiffTime)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock ( getCurrentTime )
import           Data.Time.Clock.POSIX ( posixSecondsToUTCTime
                                       , utcTimeToPOSIXSeconds )
import           Network.Connection (ConnectionContext, Connection
                                    , initConnectionContext, connectionClose)
import           Network.Mattermost.Types.Base
import           Network.Mattermost.Types.Internal
import           Network.Mattermost.Util (mkConnection)

runLogger :: ConnectionData -> String -> LogEventType -> IO ()
runLogger ConnectionData { cdLogger = Just l } n ev =
  l (LogEvent n ev)
runLogger _ _ _ = return ()

runLoggerS :: Session -> String -> LogEventType -> IO ()
runLoggerS (Session cd _) = runLogger cd

maybeFail :: Parser a -> Parser (Maybe a)
maybeFail p = (Just <$> p) <|> (return Nothing)

-- | Creates a structure representing a TLS connection to the server.
mkConnectionData :: Hostname -> Port -> Pool.Pool Connection -> ConnectionContext -> ConnectionData
mkConnectionData host port pool ctx = ConnectionData
  { cdHostname       = host
  , cdPort           = port
  , cdConnectionCtx  = ctx
  , cdAutoClose      = No
  , cdConnectionPool = pool
  , cdToken          = Nothing
  , cdLogger         = Nothing
  , cdUseTLS         = True
  }

-- | Plaintext HTTP instead of a TLS connection.
mkConnectionDataInsecure :: Hostname -> Port -> Pool.Pool Connection -> ConnectionContext -> ConnectionData
mkConnectionDataInsecure host port pool ctx = ConnectionData
  { cdHostname       = host
  , cdPort           = port
  , cdConnectionCtx  = ctx
  , cdAutoClose      = No
  , cdConnectionPool = pool
  , cdToken          = Nothing
  , cdLogger         = Nothing
  , cdUseTLS         = False
  }

createPool :: Hostname -> Port -> ConnectionContext -> ConnectionPoolConfig -> IO (Pool.Pool Connection)
createPool host port ctx cpc =
  Pool.createPool (mkConnection ctx host port True) connectionClose
                  (cpStripesCount cpc) (cpIdleConnTimeout cpc) (cpMaxConnCount cpc)

initConnectionData :: Hostname -> Port -> ConnectionPoolConfig -> IO ConnectionData
initConnectionData host port cpc = do
  ctx  <- initConnectionContext
  pool <- createPool host port ctx cpc
  return (mkConnectionData host port pool ctx)

initConnectionDataInsecure :: Hostname -> Port -> ConnectionPoolConfig -> IO ConnectionData
initConnectionDataInsecure host port cpc = do
  ctx  <- initConnectionContext
  pool <- createPool host port ctx cpc
  return (mkConnectionDataInsecure host port pool ctx)

destroyConnectionData :: ConnectionData -> IO ()
destroyConnectionData = Pool.destroyAllResources . cdConnectionPool

withLogger :: ConnectionData -> Logger -> ConnectionData
withLogger cd logger = cd { cdLogger = Just logger }

noLogger :: ConnectionData -> ConnectionData
noLogger cd = cd { cdLogger = Nothing }

data ConnectionPoolConfig = ConnectionPoolConfig
  { cpStripesCount    :: Int
  , cpIdleConnTimeout :: NominalDiffTime
  , cpMaxConnCount    :: Int
  }

defaultConnectionPoolConfig :: ConnectionPoolConfig
defaultConnectionPoolConfig = ConnectionPoolConfig 1 30 5

data Session = Session
  { sessConn :: ConnectionData
  , sessTok  :: Token
  }

data Login
  = Login
  { username :: Text
  , password :: Text
  }

instance A.ToJSON Login where
  toJSON l = A.object ["login_id" A..= username l
                      ,"password" A..= password l
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

data SearchPosts = SearchPosts
 { searchPostsTerms      :: Text
 , searchPostsIsOrSearch :: Bool
 }

instance A.ToJSON SearchPosts where
 toJSON (SearchPosts t os) =
     A.object ["terms" A..= t
              ,"is_or_search" A..= os
              ]

data Type = Ordinary
          | Direct
          | Private
          | Group
          | Unknown Text
  deriving (Read, Show, Ord, Eq)

instance A.FromJSON Type where
  parseJSON = A.withText "Type" $ \t ->
      return $ if | t == "O"  -> Ordinary
                  | t == "D"  -> Direct
                  | t == "P"  -> Private
                  | t == "G"  -> Group
                  | otherwise -> Unknown t

instance A.ToJSON Type where
  toJSON Direct              = A.toJSON ("D"::Text)
  toJSON Ordinary            = A.toJSON ("O"::Text)
  toJSON Private             = A.toJSON ("P"::Text)
  toJSON Group     = A.toJSON ("G"::Text)
  toJSON (Unknown t)         = A.toJSON t

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

instance PrintfArg TeamId where
  formatArg = formatArg . idString

data Team
  = Team
  { teamId              :: TeamId
  , teamCreateAt        :: ServerTime
  , teamUpdateAt        :: ServerTime
  , teamDeleteAt        :: ServerTime
  , teamDisplayName     :: Text
  , teamName            :: Text
  , teamEmail           :: Text
  , teamType            :: Type
  , teamCompanyName     :: Text
  , teamAllowedDomains  :: Text
  , teamInviteId        :: Id
  , teamAllowOpenInvite :: Bool
  }
  deriving (Read, Show, Eq, Ord)

instance HasId Team TeamId where
  getId = teamId

instance A.FromJSON Team where
  parseJSON = A.withObject "Team" $ \v -> do
    teamId              <- v .: "id"
    teamCreateAt        <- timeFromServer <$> v .: "create_at"
    teamUpdateAt        <- timeFromServer <$> v .: "update_at"
    teamDeleteAt        <- timeFromServer <$> v .: "delete_at"
    teamDisplayName     <- v .: "display_name"
    teamName            <- v .: "name"
    teamEmail           <- v .: "email"
    teamType            <- v .: "type"
    teamCompanyName     <- v .: "company_name"
    teamAllowedDomains  <- v .: "allowed_domains"
    teamInviteId        <- v .: "invite_id"
    teamAllowOpenInvite <- v .: "allow_open_invite"
    return Team { .. }

data TeamMember = TeamMember
  { teamMemberUserId :: UserId
  , teamMemberTeamId :: TeamId
  , teamMemberRoles  :: Text
  } deriving (Read, Show, Eq, Ord)

instance A.FromJSON TeamMember where
  parseJSON = A.withObject "TeamMember" $ \v -> do
    teamMemberUserId <- v .: "user_id"
    teamMemberTeamId <- v .: "team_id"
    teamMemberRoles  <- v .: "roles"
    return TeamMember { .. }

instance A.ToJSON TeamMember where
  toJSON TeamMember { .. } = A.object
    [ "user_id" .= teamMemberUserId
    , "team_id" .= teamMemberTeamId
    , "roles"   .= teamMemberRoles
    ]
--

data WithDefault a
  = IsValue a
  | Default
    deriving (Read, Show, Eq, Ord)

instance A.ToJSON t => A.ToJSON (WithDefault t) where
  toJSON Default = A.String "default"
  toJSON (IsValue x) = A.toJSON x

instance A.FromJSON t => A.FromJSON (WithDefault t) where
  parseJSON (A.String "default") = return Default
  parseJSON t                    = IsValue <$> A.parseJSON t

instance Functor WithDefault where
  fmap f (IsValue x) = IsValue (f x)
  fmap _ Default     = Default

data NotifyOption
  = NotifyOptionAll
  | NotifyOptionMention
  | NotifyOptionNone
    deriving (Read, Show, Eq, Ord)

instance A.ToJSON NotifyOption where
  toJSON NotifyOptionAll     = A.String "all"
  toJSON NotifyOptionMention = A.String "mention"
  toJSON NotifyOptionNone    = A.String "none"

instance A.FromJSON NotifyOption where
  parseJSON (A.String "all")     = return NotifyOptionAll
  parseJSON (A.String "mention") = return NotifyOptionMention
  parseJSON (A.String "none")    = return NotifyOptionNone
  parseJSON xs                   = fail ("Unknown NotifyOption value: " ++ show xs)

data UserNotifyProps = UserNotifyProps
  { userNotifyPropsMentionKeys  :: [Text]
  , userNotifyPropsEmail        :: Bool
  , userNotifyPropsPush         :: NotifyOption
  , userNotifyPropsDesktop      :: NotifyOption
  , userNotifyPropsDesktopSound :: Bool
  , userNotifyPropsChannel      :: Bool
  , userNotifyPropsFirstName    :: Bool
  } deriving (Eq, Show, Read, Ord)

data ChannelNotifyProps = ChannelNotifyProps
  { channelNotifyPropsEmail      :: WithDefault Bool
  , channelNotifyPropsDesktop    :: WithDefault NotifyOption
  , channelNotifyPropsPush       :: WithDefault NotifyOption
  , channelNotifyPropsMarkUnread :: WithDefault NotifyOption
  } deriving (Eq, Show, Read, Ord)

emptyUserNotifyProps :: UserNotifyProps
emptyUserNotifyProps = UserNotifyProps
  { userNotifyPropsMentionKeys  = []
  , userNotifyPropsEmail        = False
  , userNotifyPropsPush         = NotifyOptionNone
  , userNotifyPropsDesktop      = NotifyOptionNone
  , userNotifyPropsDesktopSound = False
  , userNotifyPropsChannel      = False
  , userNotifyPropsFirstName    = False
  }

emptyChannelNotifyProps :: ChannelNotifyProps
emptyChannelNotifyProps = ChannelNotifyProps
  { channelNotifyPropsEmail      = Default
  , channelNotifyPropsPush       = Default
  , channelNotifyPropsDesktop    = Default
  , channelNotifyPropsMarkUnread = Default
  }

newtype BoolString = BoolString { fromBoolString :: Bool }

instance A.FromJSON BoolString where
  parseJSON = A.withText "bool as string" $ \v ->
    case v of
      "true"  -> return (BoolString True)
      "false" -> return (BoolString False)
      _       -> fail "Expected \"true\" or \"false\""

instance A.ToJSON BoolString where
  toJSON (BoolString True) = A.String "true"
  toJSON (BoolString False) = A.String "false"

instance A.FromJSON UserNotifyProps where
  parseJSON = A.withObject "UserNotifyProps" $ \v -> do
    userNotifyPropsMentionKeys  <- T.split (==',') <$>
                                     (v .:? "mention_keys" .!= "")
    userNotifyPropsPush         <- v .:? "push" .!= NotifyOptionMention
    userNotifyPropsDesktop      <- v .:? "desktop" .!= NotifyOptionAll
    userNotifyPropsEmail        <- fromBoolString <$> (v .:? "email"         .!= BoolString True)
    userNotifyPropsDesktopSound <- fromBoolString <$> (v .:? "desktop_sound" .!= BoolString True)
    userNotifyPropsChannel      <- fromBoolString <$> (v .:? "channel"       .!= BoolString True)
    userNotifyPropsFirstName    <- fromBoolString <$> (v .:? "first_name"    .!= BoolString False)
    return UserNotifyProps { .. }

instance A.ToJSON UserNotifyProps where
  toJSON UserNotifyProps { .. } = A.object
    [ "mention_keys"  .= T.intercalate "," userNotifyPropsMentionKeys
    , "push"          .= userNotifyPropsPush
    , "desktop"       .= userNotifyPropsDesktop
    , "email"         .= BoolString userNotifyPropsEmail
    , "desktop_sound" .= BoolString userNotifyPropsDesktopSound
    , "channel"       .= BoolString userNotifyPropsChannel
    , "first_name"    .= BoolString userNotifyPropsFirstName
    ]

instance A.FromJSON ChannelNotifyProps where
  parseJSON = A.withObject "ChannelNotifyProps" $ \v -> do
    channelNotifyPropsEmail      <- fmap fromBoolString <$>
                                    (v .:? "email" .!= IsValue (BoolString True))
    channelNotifyPropsPush       <- v .:? "push" .!= IsValue NotifyOptionMention
    channelNotifyPropsDesktop    <- v .:? "desktop" .!= IsValue NotifyOptionAll
    channelNotifyPropsMarkUnread <- v .:? "mark_unread" .!= IsValue NotifyOptionAll
    return ChannelNotifyProps { .. }

instance A.ToJSON ChannelNotifyProps where
  toJSON ChannelNotifyProps { .. } = A.object
    [ "email"       .= fmap BoolString channelNotifyPropsEmail
    , "push"        .= channelNotifyPropsPush
    , "desktop"     .= channelNotifyPropsDesktop
    , "mark_unread" .= channelNotifyPropsMarkUnread
    ]

--

newtype ChannelId = CI { unCI :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

instance IsId ChannelId where
  toId   = unCI
  fromId = CI

instance PrintfArg ChannelId where
  formatArg = formatArg . idString

data Channel
  = Channel
  { channelId            :: ChannelId
  , channelCreateAt      :: ServerTime
  , channelUpdateAt      :: ServerTime
  , channelDeleteAt      :: ServerTime
  , channelTeamId        :: Maybe TeamId
  , channelType          :: Type
  , channelDisplayName   :: Text
  , channelName          :: Text
  , channelHeader        :: Text
  , channelPurpose       :: Text
  , channelLastPostAt    :: ServerTime
  , channelTotalMsgCount :: Int
  , channelExtraUpdateAt :: ServerTime
  , channelCreatorId     :: Maybe UserId
  } deriving (Read, Show, Eq, Ord)

instance HasId Channel ChannelId where
  getId = channelId

instance A.FromJSON Channel where
  parseJSON = A.withObject "Channel" $ \v -> do
    channelId              <- v .: "id"
    channelCreateAt        <- timeFromServer <$> v .: "create_at"
    channelUpdateAt        <- timeFromServer <$> v .: "update_at"
    channelDeleteAt        <- timeFromServer <$> v .: "delete_at"
    channelTeamId          <- maybeFail (v .: "team_id")
    channelType            <- v .: "type"
    channelDisplayName     <- v .: "display_name"
    channelName            <- v .: "name"
    channelHeader          <- v .: "header"
    channelPurpose         <- v .: "purpose"
    channelLastPostAt      <- timeFromServer <$> v .: "last_post_at"
    channelTotalMsgCount   <- v .: "total_msg_count"
    channelExtraUpdateAt   <- timeFromServer <$> v .: "extra_update_at"
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
  , channelDataRoles        :: Text
  , channelDataLastViewedAt :: ServerTime
  , channelDataMsgCount     :: Int
  , channelDataMentionCount :: Int
  , channelDataNotifyProps  :: ChannelNotifyProps
  , channelDataLastUpdateAt :: ServerTime
  } deriving (Read, Show, Eq)

instance A.FromJSON ChannelData where
  parseJSON = A.withObject "ChannelData" $ \o -> do
    channelDataChannelId <- o .: "channel_id"
    channelDataUserId    <- o .: "user_id"
    channelDataRoles     <- o .: "roles"
    channelDataLastViewedAt <- timeFromServer <$> o .: "last_viewed_at"
    channelDataMsgCount     <- o .: "msg_count"
    channelDataMentionCount <- o .: "mention_count"
    channelDataNotifyProps  <- o .: "notify_props"
    channelDataLastUpdateAt <- timeFromServer <$> o .: "last_update_at"
    return ChannelData { .. }

data ChannelWithData = ChannelWithData Channel ChannelData
  deriving (Read, Show, Eq)

instance A.FromJSON ChannelWithData where
  parseJSON (A.Object v) =
      ChannelWithData <$> (v .: "channel")
                      <*> (v .: "member")
  parseJSON v = typeMismatch "Invalid channel/data pair " v

type Channels = Seq Channel

data MinChannel = MinChannel
  { minChannelName        :: Text
  , minChannelDisplayName :: Text
  , minChannelPurpose     :: Maybe Text
  , minChannelHeader      :: Maybe Text
  , minChannelType        :: Type
  , minChannelTeamId      :: TeamId
  } deriving (Read, Eq, Show)

instance A.ToJSON MinChannel where
  toJSON MinChannel { .. }  = A.object $
    [ "name"         .= minChannelName
    , "display_name" .= minChannelDisplayName
    , "type"         .= minChannelType
    , "team_id"      .= minChannelTeamId
    ] ++
    [ "purpose" .= p | Just p <- [minChannelPurpose] ] ++
    [ "header"  .= h | Just h <- [minChannelHeader] ]
--

newtype UserId = UI { unUI :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

instance IsId UserId where
  toId   = unUI
  fromId = UI

instance PrintfArg UserId where
  formatArg = formatArg . idString

data UserParam
  = UserById UserId
  | UserMe
  deriving (Read, Show, Eq, Ord)

instance PrintfArg UserParam where
  formatArg = formatArg . userParamString

userParamString :: UserParam -> Text
userParamString (UserById uid) = idString uid
userParamString UserMe         = "me"

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
  , userCreateAt           :: ServerTime
  , userUpdateAt           :: ServerTime
  , userDeleteAt           :: ServerTime
  , userUsername           :: Text
  , userAuthData           :: Text
  , userAuthService        :: Text
  , userEmail              :: Text
  , userEmailVerified      :: Bool
  , userNickname           :: Text
  , userFirstName          :: Text
  , userLastName           :: Text
  , userRoles              :: Text
  , userNotifyProps        :: UserNotifyProps
  , userLastPasswordUpdate :: Maybe ServerTime
  , userLastPictureUpdate  :: Maybe ServerTime
  , userLocale             :: Text
  } deriving (Read, Show, Eq)

instance A.FromJSON User where
  parseJSON = A.withObject "User" $ \o -> do
    userId                 <- o .: "id"
    userCreateAt           <- timeFromServer <$> o .: "create_at"
    userUpdateAt           <- timeFromServer <$> o .: "update_at"
    userDeleteAt           <- timeFromServer <$> o .: "delete_at"
    userUsername           <- o .:  "username"
    userAuthData           <- o .:  "auth_data"
    userAuthService        <- o .:  "auth_service"
    userEmail              <- o .:  "email"
    userEmailVerified      <- o .:? "email_verified" .!= False
    userNickname           <- o .:  "nickname"
    userFirstName          <- o .:  "first_name"
    userLastName           <- o .:  "last_name"
    userRoles              <- o .:  "roles"
    userNotifyProps        <- o .:? "notify_props" .!= emptyUserNotifyProps
    userLastPasswordUpdate <- (timeFromServer <$>) <$>
                              (o .:? "last_password_update")
    userLastPictureUpdate  <- (timeFromServer <$>) <$> (o .:? "last_picture_update")
    userLocale             <- o .: "locale"
    return User { .. }

-- The PostPropAttachment and PostPropAttachmentField types are
-- actually defined by Slack, and simply used by Mattermost; the
-- description of these fields can be found in this document:
-- https://api.slack.com/docs/message-attachments

data PostPropAttachmentField = PostPropAttachmentField
  { ppafTitle :: Text
  , ppafValue :: Text
  , ppafShort :: Bool
  } deriving (Read, Show, Eq)

instance A.FromJSON PostPropAttachmentField where
  parseJSON = A.withObject "PostPropAttachmentField" $ \v -> do
    ppafTitle <- v .: "title"
    ppafValue <- v .: "value"
    ppafShort <- v .: "short"
    return PostPropAttachmentField { .. }

data PostPropAttachment
  = PostPropAttachment
  { ppaId         :: Int
  , ppaFallback   :: Text
  , ppaColor      :: Text
  , ppaPretext    :: Text
  , ppaAuthorName :: Text
  , ppaAuthorLink :: Text
  , ppaAuthorIcon :: Text
  , ppaTitle      :: Text
  , ppaTitleLink  :: Text
  , ppaText       :: Text
  , ppaFields     :: Seq PostPropAttachmentField
  , ppaImageURL   :: Text
  , ppaThumbURL   :: Text
  , ppaFooter     :: Text
  , ppaFooterIcon :: Text
  } deriving (Read, Show, Eq)

instance A.FromJSON PostPropAttachment where
  parseJSON = A.withObject "Attachment" $ \v -> do
    let x .:?? f = x .: f <|> return mempty
    ppaId         <- v .: "id" <|> return 0
    ppaFallback   <- v .:?? "fallback"
    ppaColor      <- v .:?? "color"
    ppaPretext    <- v .:?? "pretext"
    ppaAuthorName <- v .:?? "author_name"
    ppaAuthorLink <- v .:?? "author_link"
    ppaAuthorIcon <- v .:?? "author_icon"
    ppaTitle      <- v .:?? "title"
    ppaTitleLink  <- v .:?? "title_link"
    ppaText       <- v .:?? "text"
    ppaFields     <- v .:?? "fields"
    ppaImageURL   <- v .:?? "image_url"
    ppaThumbURL   <- v .:?? "thumb_url"
    ppaFooter     <- v .:?? "footer"
    ppaFooterIcon <- v .:?? "footer_icon"
    return PostPropAttachment { .. }

instance A.ToJSON PostPropAttachment where
  toJSON PostPropAttachment { .. } = A.object
    [ "color" .= ppaColor
    , "text"  .= ppaText
    ]

data PostProps
  = PostProps
  { postPropsOverrideIconUrl  :: Maybe Text
  , postPropsOverrideUsername :: Maybe Text
  , postPropsAttachments      :: Maybe (Seq PostPropAttachment) -- A.Value
  , postPropsNewHeader        :: Maybe Text
  , postPropsOldHeader        :: Maybe Text
  } deriving (Read, Show, Eq)

instance A.FromJSON PostProps where
  parseJSON = A.withObject "Props" $ \v -> do
    postPropsOverrideIconUrl  <- v .:? "override_icon_url"
    postPropsOverrideUsername <- v .:? "override_username"
    postPropsAttachments      <- v .:? "attachments"
    postPropsNewHeader        <- v .:? "new_header"
    postPropsOldHeader        <- v .:? "old_header"
    return PostProps { .. }

instance A.ToJSON PostProps where
  toJSON PostProps { .. } = A.object $
    [ "override_icon_url" .= v | Just v <- [postPropsOverrideIconUrl ] ] ++
    [ "override_username" .= v | Just v <- [postPropsOverrideUsername] ] ++
    [ "attachments"       .= v | Just v <- [postPropsAttachments     ] ] ++
    [ "new_header"        .= v | Just v <- [postPropsNewHeader       ] ] ++
    [ "old_header"        .= v | Just v <- [postPropsOldHeader       ] ]

newtype PostId = PI { unPI :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

instance IsId PostId where
  toId   = unPI
  fromId = PI

instance PrintfArg PostId where
  formatArg = formatArg . idString

newtype FileId = FI { unFI :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

instance IsId FileId where
  toId = unFI
  fromId = FI

instance PrintfArg FileId where
  formatArg = formatArg . idString

urlForFile :: FileId -> Text
urlForFile fId =
  "/api/v3/files/" <> idString fId <> "/get"

data PostType
  = PostTypeJoinChannel
  | PostTypeLeaveChannel
  | PostTypeAddToChannel
  | PostTypeRemoveFromChannel
  | PostTypeHeaderChange
  | PostTypeDisplayNameChange
  | PostTypePurposeChange
  | PostTypeChannelDeleted
  | PostTypeEphemeral
  | PostTypeUnknown T.Text
    deriving (Read, Show, Eq)

instance A.FromJSON PostType where
  parseJSON = A.withText "Post type" $ \ t -> return $ case t of
    "system_join_channel"        -> PostTypeJoinChannel
    "system_leave_channel"       -> PostTypeLeaveChannel
    "system_add_to_channel"      -> PostTypeAddToChannel
    "system_remove_from_channel" -> PostTypeRemoveFromChannel
    "system_header_change"       -> PostTypeHeaderChange
    "system_displayname_change"  -> PostTypeDisplayNameChange
    "system_purpose_change"      -> PostTypePurposeChange
    "system_channel_deleted"     -> PostTypeChannelDeleted
    "system_ephemeral"           -> PostTypeEphemeral
    _                            -> PostTypeUnknown t

instance A.ToJSON PostType where
  toJSON typ = A.String $ case typ of
    PostTypeJoinChannel       -> "system_join_channel"
    PostTypeLeaveChannel      -> "system_leave_channel"
    PostTypeAddToChannel      -> "system_add_to_channel"
    PostTypeRemoveFromChannel -> "system_remove_from_channel"
    PostTypeHeaderChange      -> "system_header_change"
    PostTypeDisplayNameChange -> "system_displayname_change"
    PostTypePurposeChange     -> "system_purpose_change"
    PostTypeChannelDeleted    -> "system_channel_deleted"
    PostTypeEphemeral         -> "system_ephemeral"
    PostTypeUnknown t         -> t

data Post
  = Post
  { postPendingPostId :: Maybe PostId
  , postOriginalId    :: Maybe PostId
  , postProps         :: PostProps
  , postRootId        :: Maybe PostId
  , postFileIds       :: Seq FileId
  , postId            :: PostId
  , postType          :: PostType
  , postMessage       :: Text
  , postDeleteAt      :: Maybe ServerTime
  , postHashtags      :: Text
  , postUpdateAt      :: ServerTime
  , postEditAt        :: ServerTime
  , postUserId        :: Maybe UserId
  , postCreateAt      :: ServerTime
  , postParentId      :: Maybe PostId
  , postChannelId     :: ChannelId
  , postHasReactions  :: Bool
  } deriving (Read, Show, Eq)

instance HasId Post PostId where
  getId = postId

instance A.FromJSON Post where
  parseJSON = A.withObject "Post" $ \v -> do
    postPendingPostId <- maybeFail (v .: "pending_post_id")
    postOriginalId    <- maybeFail (v .: "original_id")
    postProps         <- v .: "props"
    postRootId        <- maybeFail (v .: "root_id")
    postFileIds       <- (v .: "file_ids") <|> (return mempty)
    postId            <- v .: "id"
    postType          <- v .: "type"
    postMessage       <- v .: "message"
    postDeleteAt      <- (timeFromServer <$>) <$> v .:? "delete_at"
    postHashtags      <- v .: "hashtags"
    postUpdateAt      <- timeFromServer <$> v .: "update_at"
    postEditAt        <- timeFromServer <$> v .: "edit_at"
    postUserId        <- maybeFail (v .: "user_id")
    postCreateAt      <- timeFromServer <$> v .: "create_at"
    postParentId      <- maybeFail (v .: "parent_id")
    postChannelId     <- v .: "channel_id"
    postHasReactions  <- (v .: "has_reactions") <|> (return False)
    return Post { .. }

instance A.ToJSON Post where
  toJSON Post { .. } = A.object
    [ "pending_post_id" .= postPendingPostId
    , "original_id"     .= postOriginalId
    , "props"           .= postProps
    , "root_id"         .= postRootId
    , "file_ids"        .= postFileIds
    , "id"              .= postId
    , "type"            .= postType
    , "message"         .= postMessage
    , "delete_at"       .= (timeToServer <$> postDeleteAt)
    , "hashtags"        .= postHashtags
    , "update_at"       .= timeToServer postUpdateAt
    , "user_id"         .= postUserId
    , "create_at"       .= timeToServer postCreateAt
    , "parent_id"       .= postParentId
    , "channel_id"      .= postChannelId
    , "has_reactions"   .= postHasReactions
    ]

data PendingPost
  = PendingPost
  { pendingPostChannelId :: ChannelId
  , pendingPostCreateAt  :: Maybe ServerTime
  , pendingPostFilenames :: Seq FilePath
  , pendingPostMessage   :: Text
  , pendingPostId        :: PendingPostId
  , pendingPostUserId    :: UserId
  , pendingPostParentId  :: Maybe PostId
  , pendingPostRootId    :: Maybe PostId
  } deriving (Read, Show, Eq)

instance A.ToJSON PendingPost where
  toJSON post = A.object
    [ "channel_id"      .= pendingPostChannelId post
    , "create_at"       .= maybe 0 timeToServer (pendingPostCreateAt post)
    , "filenames"       .= pendingPostFilenames post
    , "message"         .= pendingPostMessage   post
    , "pending_post_id" .= pendingPostId        post
    , "user_id"         .= pendingPostUserId    post
    , "root_id"         .= pendingPostRootId    post
    , "parent_id"       .= pendingPostParentId  post
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
  -- locally generating a ServerTime: ok because it's just used for an
  -- initial string ID for this post and not an actual time value.
  now <- getCurrentTime
  let ms  = timeToServer (ServerTime now) :: Int
      pid = T.pack $ printf "%s:%d" (idString userid) ms
  return PendingPost
    { pendingPostId        = PPI (Id pid)
    , pendingPostChannelId = channelid
    , pendingPostCreateAt  = Nothing
    , pendingPostFilenames = S.empty
    , pendingPostMessage   = msg
    , pendingPostUserId    = userid
    , pendingPostRootId    = Nothing
    , pendingPostParentId  = Nothing
    }

data FileInfo
  = FileInfo
  { fileInfoId         :: FileId
  , fileInfoUserId     :: UserId
  , fileInfoPostId     :: Maybe PostId
  , fileInfoCreateAt   :: ServerTime
  , fileInfoUpdateAt   :: ServerTime
  , fileInfoDeleteAt   :: ServerTime
  , fileInfoName       :: Text
  , fileInfoExtension  :: Text
  , fileInfoSize       :: Int
  , fileInfoMimeType   :: Text
  , fileInfoWidth      :: Maybe Int
  , fileInfoHeight     :: Maybe Int
  , fileInfoHasPreview :: Bool
  } deriving (Read, Show, Eq)

instance ToJSON FileInfo where
  toJSON = error "file info"

instance FromJSON FileInfo where
  parseJSON = A.withObject "file_info" $ \o -> do
    fileInfoId         <- o .: "id"
    fileInfoUserId     <- o .: "user_id"
    fileInfoPostId     <- o .: "post_id"
    fileInfoCreateAt   <- timeFromServer <$> o .: "create_at"
    fileInfoUpdateAt   <- timeFromServer <$> o .: "update_at"
    fileInfoDeleteAt   <- timeFromServer <$> o .: "delete_at"
    fileInfoName       <- o .: "name"
    fileInfoExtension  <- o .: "extension"
    fileInfoSize       <- o .: "size"
    fileInfoMimeType   <- o .: "mime_type"
    fileInfoWidth      <- o .:? "width"
    fileInfoHeight     <- o .:? "height"
    fileInfoHasPreview <- (o .: "has_preview_image") <|> pure False
    return FileInfo { .. }

--

data Posts
  = Posts
  { postsPosts :: HM.HashMap PostId Post
  , postsOrder :: Seq PostId
  } deriving (Read, Show, Eq)

instance A.FromJSON Posts where
  parseJSON = A.withObject "Posts" $ \v -> do
    postsPosts <- v .:? "posts" .!= HM.empty
    postsOrder <- v .:  "order"
    return Posts { .. }

--

-- The JSON specification of times exchanged with the server are in
-- integer milliSeconds; convert to and from the local ServerTime
-- internal value.

timeFromServer :: Integer -> ServerTime
timeFromServer ms = ServerTime $ posixSecondsToUTCTime (fromRational (ms%1000))

timeToServer :: ServerTime -> Int
timeToServer time = truncate ((utcTimeToPOSIXSeconds $ withServerTime time)*1000)

--

data MinCommand
  = MinCommand
  { minComChannelId :: ChannelId
  , minComCommand   :: Text
  , minComParentId  :: Maybe PostId
  , minComRootId    :: Maybe PostId
  , minComTeamId    :: TeamId
  } deriving (Read, Show, Eq)

instance A.ToJSON MinCommand where
  toJSON MinCommand { .. } = A.object
    [ "channel_id" .= minComChannelId
    , "command"   .= minComCommand
    , "parent_id" .= minComParentId
    , "root_id" .= minComRootId
    , "team_id" .= minComTeamId
    ]

--

data Command
  = Command
  { commandId               :: CommandId
  , commandToken            :: Token
  , commandCreateAt         :: ServerTime
  , commandUpdateAt         :: ServerTime
  , commandDeleteAt         :: ServerTime
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

instance PrintfArg CommandId where
  formatArg = formatArg . idString

data CommandResponseType
  = CommandResponseInChannel
  | CommandResponseEphemeral
    deriving (Read, Show, Eq)

instance A.FromJSON CommandResponseType where
  parseJSON (A.String "in_channel") = return CommandResponseInChannel
  parseJSON (A.String "ephemeral")  = return CommandResponseEphemeral
  parseJSON _ = fail "Unknown command response type: expected `in_channel` or `ephemeral`"

data CommandResponse
  = CommandResponse
  { commandResponseType         :: Maybe CommandResponseType
  , commandResponseText         :: Text
  , commandResponseUsername     :: Text
  , commandResponseIconURL      :: Text
  , commandResponseGotoLocation :: Text
  , commandResponseAttachments  :: Seq PostPropAttachment
  } deriving (Read, Show, Eq)

instance A.FromJSON CommandResponse where
  parseJSON = A.withObject "CommandResponse" $ \o -> do
    commandResponseType         <- optional (o .: "response_type")
    commandResponseText         <- o .: "text"
    commandResponseUsername     <- o .: "username"
    commandResponseIconURL      <- o .: "icon_url"
    commandResponseGotoLocation <- o .: "goto_location"
    commandResponseAttachments  <- o .:? "attachments" .!= S.empty
    return CommandResponse { .. }

--

data UsersCreate
  = UsersCreate
  { usersCreateEmail          :: Text
  , usersCreatePassword       :: Text
  , usersCreateUsername       :: Text
  , usersCreateAllowMarketing :: Bool
  } deriving (Read, Show, Eq)

instance A.ToJSON UsersCreate where
  toJSON UsersCreate { .. } = A.object
    [ "email"           .= usersCreateEmail
    , "allow_marketing" .= usersCreateAllowMarketing
    , "password"        .= usersCreatePassword
    , "username"        .= usersCreateUsername
    ]

--

data TeamsCreate
  = TeamsCreate
  { teamsCreateDisplayName :: Text
  , teamsCreateName        :: Text
  , teamsCreateType        :: Type
  } deriving (Read, Show, Eq)

instance A.ToJSON TeamsCreate where
  toJSON TeamsCreate { .. } = A.object
    [ "display_name" .= teamsCreateDisplayName
    , "name"         .= teamsCreateName
    , "type"         .= teamsCreateType
    ]

--

data Reaction
  = Reaction
  { reactionUserId    :: UserId
  , reactionPostId    :: PostId
  , reactionEmojiName :: Text
  , reactionCreateAt  :: ServerTime
  } deriving (Read, Show, Eq)

instance A.FromJSON Reaction where
  parseJSON = A.withObject "Reaction" $ \v -> do
    reactionUserId    <- v .: "user_id"
    reactionPostId    <- v .: "post_id"
    reactionEmojiName <- v .: "emoji_name"
    reactionCreateAt  <- timeFromServer <$> v .: "create_at"
    return Reaction { .. }

instance A.ToJSON Reaction where
  toJSON Reaction {.. } = A.object
    [ "user_id"    .= reactionUserId
    , "post_id"    .= reactionPostId
    , "emoji_name" .= reactionEmojiName
    , "create_at"  .= timeToServer reactionCreateAt
    ]

-- * Preferences

data PreferenceCategory
  = PreferenceCategoryDirectChannelShow
  | PreferenceCategoryGroupChannelShow
  | PreferenceCategoryTutorialStep
  | PreferenceCategoryAdvancedSettings
  | PreferenceCategoryFlaggedPost
  | PreferenceCategoryDisplaySettings
  | PreferenceCategoryTheme
  | PreferenceCategoryAuthorizedOAuthApp
  | PreferenceCategoryNotifications
  | PreferenceCategoryLast
  | PreferenceCategoryOther Text
    deriving (Read, Show, Eq)

instance A.FromJSON PreferenceCategory where
  parseJSON = A.withText "PreferenceCategory" $ \t -> return $ case t of
    "direct_channel_show" -> PreferenceCategoryDirectChannelShow
    "group_channel_show"  -> PreferenceCategoryGroupChannelShow
    "tutorial_step"       -> PreferenceCategoryTutorialStep
    "advanced_settings"   -> PreferenceCategoryAdvancedSettings
    "flagged_post"        -> PreferenceCategoryFlaggedPost
    "display_settings"    -> PreferenceCategoryDisplaySettings
    "theme"               -> PreferenceCategoryTheme
    "oauth_app"           -> PreferenceCategoryAuthorizedOAuthApp
    "notifications"       -> PreferenceCategoryNotifications
    "last"                -> PreferenceCategoryLast
    _                     -> PreferenceCategoryOther t

instance A.ToJSON PreferenceCategory where
  toJSON cat = A.String $ case cat of
    PreferenceCategoryDirectChannelShow  -> "direct_channel_show"
    PreferenceCategoryGroupChannelShow   -> "group_channel_show"
    PreferenceCategoryTutorialStep       -> "tutorial_step"
    PreferenceCategoryAdvancedSettings   -> "advanced_settings"
    PreferenceCategoryFlaggedPost        -> "flagged_post"
    PreferenceCategoryDisplaySettings    -> "display_settings"
    PreferenceCategoryTheme              -> "theme"
    PreferenceCategoryAuthorizedOAuthApp -> "oauth_app"
    PreferenceCategoryNotifications      -> "notifications"
    PreferenceCategoryLast               -> "last"
    PreferenceCategoryOther t            -> t

data PreferenceName
  = PreferenceName { fromRawPreferenceName :: Text }
    deriving (Read, Show, Eq)

instance A.FromJSON PreferenceName where
  parseJSON = A.withText "PreferenceValue" (return . PreferenceName)

instance A.ToJSON PreferenceName where
  toJSON = A.toJSON . fromRawPreferenceName

data PreferenceValue
  = PreferenceValue { fromRawPreferenceValue :: Text }
    deriving (Read, Show, Eq)

instance A.FromJSON PreferenceValue where
  parseJSON = A.withText "PreferenceValue" (return . PreferenceValue)

instance A.ToJSON PreferenceValue where
  toJSON = A.toJSON . fromRawPreferenceValue

data Preference
  = Preference
  { preferenceUserId   :: UserId
  , preferenceCategory :: PreferenceCategory
  , preferenceName     :: PreferenceName
  , preferenceValue    :: PreferenceValue
  } deriving (Read, Show, Eq)

instance A.FromJSON Preference where
  parseJSON = A.withObject "Preference" $ \v -> do
    preferenceUserId   <- v .: "user_id"
    preferenceCategory <- v .: "category"
    preferenceName     <- v .: "name"
    preferenceValue    <- v .: "value"
    return Preference { .. }

instance A.ToJSON Preference where
  toJSON Preference { .. } = A.object
    [ "user_id"  .= preferenceUserId
    , "category" .= preferenceCategory
    , "name"     .= preferenceName
    , "value"    .= preferenceValue
    ]

data GroupChannelPreference =
    GroupChannelPreference { groupChannelId :: ChannelId
                           , groupChannelShow :: Bool
                           } deriving (Read, Show, Eq)

-- | Attempt to expose a 'Preference' as a 'FlaggedPost'
preferenceToGroupChannelPreference :: Preference -> Maybe GroupChannelPreference
preferenceToGroupChannelPreference
  Preference
    { preferenceCategory = PreferenceCategoryGroupChannelShow
    , preferenceName     = PreferenceName name
    , preferenceValue    = PreferenceValue value
    } = Just GroupChannelPreference
          { groupChannelId = CI (Id name)
          , groupChannelShow = value == "true"
          }
preferenceToGroupChannelPreference _ = Nothing

data FlaggedPost = FlaggedPost
  { flaggedPostUserId :: UserId
  , flaggedPostId     :: PostId
  , flaggedPostStatus :: Bool
  } deriving (Read, Show, Eq)

-- | Attempt to expose a 'Preference' as a 'FlaggedPost'
preferenceToFlaggedPost :: Preference -> Maybe FlaggedPost
preferenceToFlaggedPost
  Preference
    { preferenceCategory = PreferenceCategoryFlaggedPost
    , preferenceName     = PreferenceName name
    , preferenceValue    = PreferenceValue value
    , preferenceUserId   = userId
    } = Just FlaggedPost
          { flaggedPostUserId = userId
          , flaggedPostId     = PI (Id name)
          , flaggedPostStatus = value == "true"
          }
preferenceToFlaggedPost _ = Nothing

instance A.ToJSON FlaggedPost where
  toJSON FlaggedPost
    { flaggedPostUserId = userId
    , flaggedPostId     = PI (Id name)
    , flaggedPostStatus = status
    } = A.toJSON $ Preference
          { preferenceCategory = PreferenceCategoryFlaggedPost
          , preferenceName     = PreferenceName name
          , preferenceValue    = PreferenceValue (if status then "true" else "false")
          , preferenceUserId   = userId
          }

--

newtype HookId = HI { unHI :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

instance IsId HookId where
  toId   = unHI
  fromId = HI

instance PrintfArg HookId where
  formatArg = formatArg . idString

--

newtype InviteId = II { unII :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

instance IsId InviteId where
  toId   = unII
  fromId = II

instance PrintfArg InviteId where
  formatArg = formatArg . idString

--

newtype TokenId = TkI { unTkI :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

instance IsId TokenId where
  toId   = unTkI
  fromId = TkI

instance PrintfArg TokenId where
  formatArg = formatArg . idString

--

newtype AppId = AI { unAI :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

instance IsId AppId where
  toId   = unAI
  fromId = AI

instance PrintfArg AppId where
  formatArg = formatArg . idString

--

newtype JobId = JI { unJI :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

instance IsId JobId where
  toId   = unJI
  fromId = JI

instance PrintfArg JobId where
  formatArg = formatArg . idString

--

newtype EmojiId = EI { unEI :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

instance IsId EmojiId where
  toId   = unEI
  fromId = EI

instance PrintfArg EmojiId where
  formatArg = formatArg . idString

--

newtype ReportId = RI { unRI :: Id }
  deriving (Read, Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

instance IsId ReportId where
  toId   = unRI
  fromId = RI

instance PrintfArg ReportId where
  formatArg = formatArg . idString

-- FIXMES

instance A.ToJSON User where toJSON = error "to user"
instance A.ToJSON Team where toJSON = error "to team"
instance A.FromJSON Command where parseJSON = error "from command"
instance A.ToJSON Command where toJSON = error "to command"


-- --

data MinChannelMember = MinChannelMember
  { minChannelMemberUserId :: UserId
  , minChannelMemberChannelId :: ChannelId
  } deriving (Read, Show, Eq)

instance A.FromJSON MinChannelMember where
  parseJSON = A.withObject "channelMember" $ \v -> do
    minChannelMemberUserId <- v A..: "user_id"
    minChannelMemberChannelId <- v A..: "channel_id"
    return MinChannelMember { .. }

instance A.ToJSON MinChannelMember where
  toJSON MinChannelMember { .. } = A.object
    [ "user_id"    A..= minChannelMemberUserId
    , "channel_id" A..= minChannelMemberChannelId
    ]

data ChannelMember = ChannelMember
  { channelMemberMsgCount :: Integer
  , channelMemberUserId :: UserId
  , channelMemberRoles :: Text
  , channelMemberMentionCount :: Int
  , channelMemberLastViewedAt :: ServerTime
  , channelMemberChannelId :: ChannelId
  , channelMemberLastUpdateAt :: ServerTime
  , channelMemberNotifyProps :: ChannelNotifyProps
  } deriving (Read, Show, Eq)

instance A.FromJSON ChannelMember where
  parseJSON = A.withObject "channelMember" $ \v -> do
    channelMemberMsgCount <- v A..: "msg_count"
    channelMemberUserId <- v A..: "user_id"
    channelMemberRoles <- v A..: "roles"
    channelMemberMentionCount <- v A..: "mention_count"
    channelMemberLastViewedAt <- timeFromServer <$> v A..: "last_viewed_at"
    channelMemberChannelId <- v A..: "channel_id"
    channelMemberLastUpdateAt <- timeFromServer <$> v A..: "last_update_at"
    channelMemberNotifyProps <- v A..: "notify_props"
    return ChannelMember { .. }

instance A.ToJSON ChannelMember where
  toJSON ChannelMember { .. } = A.object
    [ "msg_count" A..= channelMemberMsgCount
    , "user_id" A..= channelMemberUserId
    , "roles" A..= channelMemberRoles
    , "mention_count" A..= channelMemberMentionCount
    , "last_viewed_at" A..= timeToServer channelMemberLastViewedAt
    , "channel_id" A..= channelMemberChannelId
    , "last_update_at" A..= timeToServer channelMemberLastUpdateAt
    , "notify_props" A..= channelMemberNotifyProps
    ]


data Status = Status
  { statusUserId :: UserId
  , statusStatus :: T.Text
  , statusManual :: Bool
  , statusLastActivityAt :: ServerTime
  }

instance A.FromJSON Status where
  parseJSON = A.withObject "Status" $ \o -> do
    statusUserId <- o A..: "user_id"
    statusStatus <- o A..: "status"
    statusManual <- o A..: "manual"
    statusLastActivityAt <- timeFromServer <$> o A..: "last_activity_at"
    return Status { .. }

instance A.ToJSON Status where
  toJSON Status { .. } = A.object
    [ "user_id" A..= statusUserId
    , "status"  A..= statusStatus
    , "manual"  A..= statusManual
    , "last_activity_at" A..= timeToServer statusLastActivityAt
    ]


data UserSearch = UserSearch
  { userSearchTerm :: Text
  , userSearchAllowInactive :: Bool
    -- ^ When `true`, include deactivated users in the results
  , userSearchWithoutTeam :: Bool
    -- ^ Set this to `true` if you would like to search for users that are not on a team. This option takes precendence over `team_id`, `in_channel_id`, and `not_in_channel_id`.
  , userSearchInChannelId :: Maybe ChannelId
    -- ^ If provided, only search users in this channel
  , userSearchNotInTeamId :: Maybe TeamId
    -- ^ If provided, only search users not on this team
  , userSearchNotInChannelId :: Maybe ChannelId
    -- ^ If provided, only search users not in this channel. Must specifiy `team_id` when using this option
  , userSearchTeamId :: Maybe TeamId
    -- ^ If provided, only search users on this team
  } deriving (Read, Show, Eq)

instance A.FromJSON UserSearch where
  parseJSON = A.withObject "userSearch" $ \v -> do
    userSearchTerm <- v A..: "term"
    userSearchAllowInactive <- v A..: "allow_inactive"
    userSearchWithoutTeam <- v A..: "without_team"
    userSearchInChannelId <- v A..: "in_channel_id"
    userSearchNotInTeamId <- v A..: "not_in_team_id"
    userSearchNotInChannelId <- v A..: "not_in_channel_id"
    userSearchTeamId <- v A..: "team_id"
    return UserSearch { .. }

instance A.ToJSON UserSearch where
  toJSON UserSearch { .. } = A.object
    [ "term" A..= userSearchTerm
    , "allow_inactive" A..= userSearchAllowInactive
    , "without_team" A..= userSearchWithoutTeam
    , "in_channel_id" A..= userSearchInChannelId
    , "not_in_team_id" A..= userSearchNotInTeamId
    , "not_in_channel_id" A..= userSearchNotInChannelId
    , "team_id" A..= userSearchTeamId
    ]

-- --

data RawPost = RawPost
  { rawPostChannelId :: ChannelId
  , rawPostMessage :: Text
    -- ^ The message contents, can be formatted with Markdown
  , rawPostFileIds :: Seq FileId
    -- ^ A list of file IDs to associate with the post
  , rawPostRootId :: Maybe PostId
    -- ^ The post ID to comment on
  } deriving (Read, Show, Eq)

instance A.FromJSON RawPost where
  parseJSON = A.withObject "rawPost" $ \v -> do
    rawPostChannelId <- v A..: "channel_id"
    rawPostMessage <- v A..: "message"
    rawPostFileIds <- v A..: "file_ids"
    rawPostRootId <- v A..:? "root_id"
    return RawPost { .. }

instance A.ToJSON RawPost where
  toJSON RawPost { .. } = A.object
    ( "channel_id" A..= rawPostChannelId
    : "message" A..= rawPostMessage
    : "file_ids" A..= rawPostFileIds
    : case rawPostRootId of
        Nothing -> []
        Just rId -> [("root_id" A..= rId)]
    )

rawPost :: Text -> ChannelId -> RawPost
rawPost message channelId = RawPost
  { rawPostChannelId = channelId
  , rawPostMessage   = message
  , rawPostFileIds   = mempty
  , rawPostRootId    = Nothing
  }


data PostUpdate = PostUpdate
  { postUpdateIsPinned :: Maybe Bool
  , postUpdateMessage :: Text
    -- ^ The message text of the post
  , postUpdateHasReactions :: Maybe Bool
    -- ^ Set to `true` if the post has reactions to it
  , postUpdateFileIds :: Maybe (Seq FileId)
    -- ^ The list of files attached to this post
  , postUpdateProps :: Maybe Text
    -- ^ A general JSON property bag to attach to the post
  } deriving (Read, Show, Eq)

instance A.FromJSON PostUpdate where
  parseJSON = A.withObject "postUpdate" $ \v -> do
    postUpdateIsPinned <- v A..:? "is_pinned" A..!= Nothing
    postUpdateMessage <- v A..: "message"
    postUpdateHasReactions <- v A..:? "has_reactions" A..!= Nothing
    postUpdateFileIds <- v A..:? "file_ids" A..!= Nothing
    postUpdateProps <- v A..:? "props" A..!= Nothing
    return PostUpdate { .. }

instance A.ToJSON PostUpdate where
  toJSON PostUpdate { .. } = A.object $
    [ "is_pinned" A..= p | Just p <- [postUpdateIsPinned] ] ++
    [ "message" A..= postUpdateMessage ] ++
    [ "has_reactions" A..= p | Just p <- [postUpdateHasReactions] ] ++
    [ "file_ids" A..= p | Just p <- [postUpdateFileIds] ] ++
    [ "props" A..= p | Just p <- [postUpdateProps] ]

postUpdate :: Text -> PostUpdate
postUpdate message = PostUpdate
  { postUpdateIsPinned = Nothing
  , postUpdateMessage = message
  , postUpdateHasReactions = Nothing
  , postUpdateFileIds = Nothing
  , postUpdateProps = Nothing
  }


data ChannelPatch = ChannelPatch
  { channelPatchHeader :: Maybe Text
  , channelPatchDisplayName :: Maybe Text
    -- ^ The non-unique UI name for the channel
  , channelPatchName :: Maybe Text
    -- ^ The unique handle for the channel, will be present in the channel URL
  , channelPatchPurpose :: Maybe Text
    -- ^ A short description of the purpose of the channel
  } deriving (Read, Show, Eq)

instance A.FromJSON ChannelPatch where
  parseJSON = A.withObject "channelPatch" $ \v -> do
    channelPatchHeader <- v A..:? "header"
    channelPatchDisplayName <- v A..:? "display_name"
    channelPatchName <- v A..:? "name"
    channelPatchPurpose <- v A..:? "purpose"
    return ChannelPatch { .. }

instance A.ToJSON ChannelPatch where
  toJSON ChannelPatch { .. } = A.object $
    [ "header" A..= x | Just x <- [ channelPatchHeader] ] ++
    [ "display_name" A..= x | Just x <- [channelPatchDisplayName] ] ++
    [ "name" A..= x | Just x <- [channelPatchName] ] ++
    [ "purpose" A..= x | Just x <- [channelPatchPurpose] ]

defaultChannelPatch :: ChannelPatch
defaultChannelPatch = ChannelPatch
  { channelPatchHeader = Nothing
  , channelPatchDisplayName = Nothing
  , channelPatchName = Nothing
  , channelPatchPurpose = Nothing
  }


data InitialTeamData = InitialTeamData
  { initialTeamDataDisplayName :: Text
  , initialTeamDataType :: Text
    -- ^ `'O'` for open, `'I'` for invite only
  , initialTeamDataName :: Text
    -- ^ Unique handler for a team, will be present in the team URL
  } deriving (Read, Show, Eq)

instance A.FromJSON InitialTeamData where
  parseJSON = A.withObject "initialTeamData" $ \v -> do
    initialTeamDataDisplayName <- v A..: "display_name"
    initialTeamDataType <- v A..: "type"
    initialTeamDataName <- v A..: "name"
    return InitialTeamData { .. }

instance A.ToJSON InitialTeamData where
  toJSON InitialTeamData { .. } = A.object
    [ "display_name" A..= initialTeamDataDisplayName
    , "type" A..= initialTeamDataType
    , "name" A..= initialTeamDataName
    ]

data ChannelStats = ChannelStats
  { channelStatsChannelId   :: Text
  , channelStatsMemberCount :: Int
  } deriving (Read, Show, Eq)

instance A.FromJSON ChannelStats where
  parseJSON = A.withObject "channelStats" $ \v -> do
    channelStatsChannelId   <- v A..: "channel_id"
    channelStatsMemberCount <- v A..: "member_count"
    return ChannelStats { .. }

instance A.ToJSON ChannelStats where
  toJSON ChannelStats { .. } = A.object
    [ "channel_id"   A..= channelStatsChannelId
    , "member_count" A..= channelStatsMemberCount
    ]

-- --

data ChannelUnread = ChannelUnread
  { channelUnreadChannelId :: Text
  , channelUnreadTeamId :: Text
  , channelUnreadMsgCount :: Int
  , channelUnreadMentionCount :: Int
  } deriving (Read, Show, Eq)

instance A.FromJSON ChannelUnread where
  parseJSON = A.withObject "channelUnread" $ \v -> do
    channelUnreadChannelId <- v A..: "channel_id"
    channelUnreadTeamId <- v A..: "team_id"
    channelUnreadMsgCount <- v A..: "msg_count"
    channelUnreadMentionCount <- v A..: "mention_count"
    return ChannelUnread { .. }

instance A.ToJSON ChannelUnread where
  toJSON ChannelUnread { .. } = A.object
    [ "channel_id" A..= channelUnreadChannelId
    , "team_id" A..= channelUnreadTeamId
    , "msg_count" A..= channelUnreadMsgCount
    , "mention_count" A..= channelUnreadMentionCount
    ]
