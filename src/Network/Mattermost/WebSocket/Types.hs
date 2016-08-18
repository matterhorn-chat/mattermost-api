{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Mattermost.WebSocket.Types
( WebsocketEventType(..)
, WebsocketEvent(..)
, WEProps(..)
) where

import           Data.Aeson ( FromJSON(..)
                            , ToJSON(..)
                            , (.:)
                            , (.:?)
                            , (.=)
                            )
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Network.WebSockets (WebSocketsData(..))

import           Network.Mattermost.Types


data WebsocketEventType
  = WMTyping
  | WMPosted
  | WMPostEdited
  | WMPostDeleted
  | WMChannelDeleted
  | WMChannelViewed
  | WMDirectAdded
  | WMNewUser
  | WMLeaveTeam
  | WMUserAdded
  | WMUserRemoved
  | WMPreferenceChanged
  | WMEphemeralMessage
  | WMStatusChange
    deriving (Read, Show, Eq, Ord)

instance FromJSON WebsocketEventType where
  parseJSON = A.withText "event type" $ \s -> case s of
    "typing"             -> return WMTyping
    "posted"             -> return WMPosted
    "post_edited"        -> return WMPostEdited
    "post_deleted"       -> return WMPostDeleted
    "channel_deleted"    -> return WMChannelDeleted
    "channel_viewed"     -> return WMChannelViewed
    "direct_added"       -> return WMDirectAdded
    "new_user"           -> return WMNewUser
    "leave_team"         -> return WMLeaveTeam
    "user_added"         -> return WMUserAdded
    "user_removed"       -> return WMUserRemoved
    "preference_changed" -> return WMPreferenceChanged
    "ephemeral_message"  -> return WMEphemeralMessage
    "status_change"      -> return WMStatusChange
    _                    -> fail ("Unknown websocket message: " ++ show s)

instance ToJSON WebsocketEventType where
  toJSON WMTyping            = "typing"
  toJSON WMPosted            = "posted"
  toJSON WMPostEdited        = "post_edited"
  toJSON WMPostDeleted       = "post_deleted"
  toJSON WMChannelDeleted    = "channel_deleted"
  toJSON WMChannelViewed     = "channel_viewed"
  toJSON WMDirectAdded       = "direct_added"
  toJSON WMNewUser           = "new_user"
  toJSON WMLeaveTeam         = "leave_team"
  toJSON WMUserAdded         = "user_added"
  toJSON WMUserRemoved       = "user_removed"
  toJSON WMPreferenceChanged = "preference_changed"
  toJSON WMEphemeralMessage  = "ephemeral_message"
  toJSON WMStatusChange      = "status_change"

--

toValueString :: ToJSON a => a -> A.Value
toValueString v =  toJSON (decodeUtf8 (toStrict (A.encode v)))

fromValueString :: FromJSON a => A.Value -> A.Parser a
fromValueString = A.withText "string-encoded json" $ \s -> do
    case A.eitherDecode (fromStrict (encodeUtf8 s)) of
      Right v  -> return v
      Left err -> fail err

--

data WebsocketEvent = WebsocketEvent
  { weTeamId    :: TeamId
  , weAction    :: WebsocketEventType
  , weUserId    :: UserId
  , weChannelId :: ChannelId
  , weProps     :: WEProps
  } deriving (Read, Show, Eq)

instance FromJSON WebsocketEvent where
  parseJSON = A.withObject "WebsocketEvent" $ \o -> do
    weTeamId    <- o .:  "team_id"
    weAction    <- o .:  "action"
    weUserId    <- o .:  "user_id"
    weChannelId <- o .:  "channel_id"
    weProps     <- o .:  "props"
    return WebsocketEvent { .. }

instance ToJSON WebsocketEvent where
  toJSON WebsocketEvent { .. } = A.object
    [ "team_id"    .= weTeamId
    , "action"     .= weAction
    , "user_id"    .= weUserId
    , "channel_id" .= weChannelId
    , "props"      .= weProps
    ]

instance WebSocketsData WebsocketEvent where
  fromLazyByteString = fromJust. A.decode
  toLazyByteString = A.encode

--

data WEProps = WEProps
  { wepChannelId          :: Maybe ChannelId
  , wepTeamId             :: Maybe TeamId
  , wepSenderName         :: Maybe Text
  , wepChannelDisplayName :: Maybe Text
  , wepPost               :: Maybe Post
  } deriving (Read, Show, Eq)

instance FromJSON WEProps where
  parseJSON = A.withObject "WebSocketEvent Props" $ \o -> do
    wepChannelId          <- o .:? "channel_id"
    wepTeamId             <- o .:? "team_id"
    wepSenderName         <- o .:? "sender_name"
    wepChannelDisplayName <- o .:? "channel_name"
    wepPostRaw            <- o .:? "post"
    wepPost <- case wepPostRaw of
      Just str -> fromValueString str
      Nothing  -> return Nothing
    return WEProps { .. }

instance ToJSON WEProps where
  toJSON WEProps { .. } = A.object
    [ "channel_id"   .= wepChannelId
    , "team_id"      .= wepTeamId
    , "sender_name"  .= wepSenderName
    , "channel_name" .= wepChannelDisplayName
    , "post"         .= toValueString wepPost
    ]

--
