{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Mattermost.WebSocket.Types
( WebsocketEventType(..)
, WebsocketEvent(..)
, WEData(..)
, WEBroadcast(..)
) where

import           Control.Applicative
import           Control.Exception ( throw )
import           Data.Aeson ( FromJSON(..)
                            , ToJSON(..)
                            , (.:)
                            , (.:?)
                            , (.=)
                            )
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int64)
import           Data.Sequence (Seq)
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Network.WebSockets (WebSocketsData(..))
import qualified Network.WebSockets as WS

import           Network.Mattermost.Types
import           Network.Mattermost.Exceptions


data WebsocketEventType
  = WMTyping
  | WMPosted
  | WMPostEdited
  | WMPostDeleted
  | WMChannelDeleted
  | WMChannelCreated
  | WMDirectAdded
  | WMGroupAdded
  | WMNewUser
  | WMAddedToTeam
  | WMLeaveTeam
  | WMUpdateTeam
  | WMUserAdded
  | WMUserUpdated
  | WMUserRemoved
  | WMPreferenceChanged
  | WMPreferenceDeleted
  | WMEphemeralMessage
  | WMStatusChange
  | WMHello
  | WMWebRTC
  | WMAuthenticationChallenge
  | WMReactionAdded
  | WMReactionRemoved
  | WMChannelViewed
  | WMChannelUpdated
  | WMEmojiAdded
  deriving (Read, Show, Eq, Ord)

instance FromJSON WebsocketEventType where
  parseJSON = A.withText "event type" $ \s -> case s of
    "typing"             -> return WMTyping
    "posted"             -> return WMPosted
    "post_edited"        -> return WMPostEdited
    "post_deleted"       -> return WMPostDeleted
    "channel_deleted"    -> return WMChannelDeleted
    "direct_added"       -> return WMDirectAdded
    "new_user"           -> return WMNewUser
    "leave_team"         -> return WMLeaveTeam
    "user_added"         -> return WMUserAdded
    "user_updated"       -> return WMUserUpdated
    "user_removed"       -> return WMUserRemoved
    "preferences_changed" -> return WMPreferenceChanged
    "ephemeral_message"  -> return WMEphemeralMessage
    "status_change"      -> return WMStatusChange
    "hello"              -> return WMHello
    "update_team"        -> return WMUpdateTeam
    "reaction_added"     -> return WMReactionAdded
    "reaction_removed"   -> return WMReactionRemoved
    "channel_created"    -> return WMChannelCreated
    "group_added"        -> return WMGroupAdded
    "added_to_team"      -> return WMAddedToTeam
    "webrtc"             -> return WMWebRTC
    "authentication_challenge" -> return WMAuthenticationChallenge
    "preferences_deleted" -> return WMPreferenceDeleted
    "channel_viewed"     -> return WMChannelViewed
    "channel_updated"    -> return WMChannelUpdated
    "emoji_added"        -> return WMEmojiAdded
    _                    -> fail ("Unknown websocket message: " ++ show s)

instance ToJSON WebsocketEventType where
  toJSON WMTyping            = "typing"
  toJSON WMPosted            = "posted"
  toJSON WMPostEdited        = "post_edited"
  toJSON WMPostDeleted       = "post_deleted"
  toJSON WMChannelDeleted    = "channel_deleted"
  toJSON WMDirectAdded       = "direct_added"
  toJSON WMNewUser           = "new_user"
  toJSON WMLeaveTeam         = "leave_team"
  toJSON WMUserAdded         = "user_added"
  toJSON WMUserUpdated       = "user_updated"
  toJSON WMUserRemoved       = "user_removed"
  toJSON WMPreferenceChanged = "preferences_changed"
  toJSON WMPreferenceDeleted = "preferences_deleted"
  toJSON WMEphemeralMessage  = "ephemeral_message"
  toJSON WMStatusChange      = "status_change"
  toJSON WMHello             = "hello"
  toJSON WMUpdateTeam        = "update_team"
  toJSON WMReactionAdded     = "reaction_added"
  toJSON WMReactionRemoved   = "reaction_removed"
  toJSON WMChannelCreated    = "channel_created"
  toJSON WMGroupAdded        = "group_added"
  toJSON WMAddedToTeam       = "added_to_team"
  toJSON WMWebRTC            = "webrtc"
  toJSON WMAuthenticationChallenge = "authentication_challenge"
  toJSON WMChannelViewed           = "channel_viewed"
  toJSON WMChannelUpdated          = "channel_updated"
  toJSON WMEmojiAdded              = "emoji_added"

--

toValueString :: ToJSON a => a -> A.Value
toValueString v =  toJSON (decodeUtf8 (toStrict (A.encode v)))

fromValueString :: FromJSON a => A.Value -> A.Parser a
fromValueString = A.withText "string-encoded json" $ \s -> do
    case A.eitherDecode (fromStrict (encodeUtf8 s)) of
      Right v  -> return v
      Left err -> throw (JSONDecodeException err (T.unpack s))

--

data WebsocketEvent = WebsocketEvent
  { weEvent     :: WebsocketEventType
  , weData      :: WEData
  , weBroadcast :: WEBroadcast
  , weSeq       :: Int64
  } deriving (Read, Show, Eq)

instance FromJSON WebsocketEvent where
  parseJSON = A.withObject "WebsocketEvent" $ \o -> do
    weEvent     <- o .: "event"
    weData      <- o .: "data"
    weBroadcast <- o .: "broadcast"
    weSeq       <- o .: "seq"
    return WebsocketEvent { .. }

instance ToJSON WebsocketEvent where
  toJSON WebsocketEvent { .. } = A.object
    [ "event"      .= weEvent
    , "data"       .= weData
    , "broadcast"  .= weBroadcast
    , "seq"        .= weSeq
    ]

instance WebSocketsData WebsocketEvent where
  fromDataMessage (WS.Text bs _) = fromLazyByteString bs
  fromDataMessage (WS.Binary bs) = fromLazyByteString bs
  fromLazyByteString s = case A.eitherDecode s of
    Left err -> throw (JSONDecodeException err (BC.unpack s))
    Right v  -> v
  toLazyByteString = A.encode

--

data WEData = WEData
  { wepChannelId          :: Maybe ChannelId
  , wepTeamId             :: Maybe TeamId
  , wepSenderName         :: Maybe Text
  , wepUserId             :: Maybe UserId
  , wepUser               :: Maybe User
  , wepChannelDisplayName :: Maybe Text
  , wepPost               :: Maybe Post
  , wepStatus             :: Maybe Text
  , wepReaction           :: Maybe Reaction
  , wepMentions           :: Maybe (Set UserId)
  , wepPreferences        :: Maybe (Seq Preference)
  } deriving (Read, Show, Eq)

instance FromJSON WEData where
  parseJSON = A.withObject "WebSocketEvent Data" $ \o -> do
    wepChannelId          <- o .:? "channel_id"
    wepTeamId             <- maybeFail (o .: "team_id")
    wepSenderName         <- o .:? "sender_name"
    wepUserId             <- o .:? "user_id"
    wepUser               <- o .:? "user"
    wepChannelDisplayName <- o .:? "channel_name"
    wepPostRaw            <- o .:? "post"
    wepPost <- case wepPostRaw of
      Just str -> fromValueString str
      Nothing  -> return Nothing
    wepStatus <- o .:? "status"
    wepReactionRaw <- o .:? "reaction"
    wepReaction <- case wepReactionRaw of
      Just str -> fromValueString str
      Nothing  -> return Nothing
    wepMentionsRaw <- o .:? "mentions"
    wepMentions <- case wepMentionsRaw of
      Just str -> fromValueString str
      Nothing  -> return Nothing
    wepPreferencesRaw <- o .:? "preferences"
    wepPreferences <- case wepPreferencesRaw of
      Just str -> fromValueString str
      Nothing  -> return Nothing
    return WEData { .. }

instance ToJSON WEData where
  toJSON WEData { .. } = A.object
    [ "channel_id"   .= wepChannelId
    , "team_id"      .= wepTeamId
    , "sender_name"  .= wepSenderName
    , "user_id"      .= wepUserId
    , "channel_name" .= wepChannelDisplayName
    , "post"         .= toValueString wepPost
    , "reaction"     .= wepReaction
    , "mentions"     .= toValueString wepMentions
    , "preferences"  .= toValueString wepPreferences
    ]

--

data WEBroadcast = WEBroadcast
  { webChannelId :: Maybe ChannelId
  , webUserId    :: Maybe UserId
  , webTeamId    :: Maybe TeamId
  , webOmitUsers :: Maybe (HM.HashMap UserId Bool)
  } deriving (Read, Show, Eq)

nullable :: Alternative f => f a -> f (Maybe a)
nullable p = (Just <$> p) <|> pure Nothing

instance FromJSON WEBroadcast where
  parseJSON = A.withObject "WebSocketEvent Broadcast" $ \o -> do
    webChannelId <- nullable (o .: "channel_id")
    webTeamId    <- nullable (o .: "team_id")
    webUserId    <- nullable (o .: "user_id")
    webOmitUsers <- nullable (o .: "omit_users")
    return WEBroadcast { .. }

instance ToJSON WEBroadcast where
  toJSON WEBroadcast { .. } = A.object
    [ "channel_id" .= webChannelId
    , "team_id"    .= webTeamId
    , "user_id"    .= webUserId
    , "omit_users" .= webOmitUsers
    ]
