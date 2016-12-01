{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Mattermost.WebSocket.Types
( WebsocketEventType(..)
, WebsocketEvent(..)
, WEData(..)
, WEBroadcast(..)
) where

import           Control.Exception ( throw )
import           Control.Applicative ((<|>))
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
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Network.WebSockets (WebSocketsData(..))

import           Network.Mattermost.Types
import           Network.Mattermost.Exceptions


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
  | WMHello
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
    "hello"              -> return WMHello
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
  toJSON WMHello             = "hello"

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
  { weTeamId    :: Maybe TeamId
  , weEvent     :: WebsocketEventType
  , weUserId    :: Maybe UserId
  , weChannelId :: Maybe ChannelId
  , weData      :: WEData
  , weBroadcast :: WEBroadcast
  } deriving (Read, Show, Eq)

instance FromJSON WebsocketEvent where
  parseJSON = A.withObject "WebsocketEvent" $ \o -> do
    weTeamId    <- (Just <$> (o .:  "team_id")) <|> return Nothing
    weEvent     <- o .:  "event"
    weUserId    <- o .:?  "user_id"
    weChannelId <- (Just <$> (o .:  "channel_id")) <|> return Nothing
    weData      <- o .: "data"
    weBroadcast <- o .: "broadcast"
    return WebsocketEvent { .. }

instance ToJSON WebsocketEvent where
  toJSON WebsocketEvent { .. } = A.object
    [ "team_id"    .= weTeamId
    , "event"      .= weEvent
    , "user_id"    .= weUserId
    , "channel_id" .= weChannelId
    , "data"       .= weData
    , "broadcast"  .= weBroadcast
    ]

instance WebSocketsData WebsocketEvent where
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
  , wepChannelDisplayName :: Maybe Text
  , wepPost               :: Maybe Post
  , wepStatus             :: Maybe Text
  } deriving (Read, Show, Eq)

instance FromJSON WEData where
  parseJSON = A.withObject "WebSocketEvent Data" $ \o -> do
    wepChannelId          <- o .:? "channel_id"
    wepTeamId             <- o .:? "team_id"
    wepSenderName         <- o .:? "sender_name"
    wepUserId             <- o .:? "user_id"
    wepChannelDisplayName <- o .:? "channel_name"
    wepPostRaw            <- o .:? "post"
    wepPost <- case wepPostRaw of
      Just str -> fromValueString str
      Nothing  -> return Nothing
    wepStatus <- o .:? "status"
    return WEData { .. }

instance ToJSON WEData where
  toJSON WEData { .. } = A.object
    [ "channel_id"   .= wepChannelId
    , "team_id"      .= wepTeamId
    , "sender_name"  .= wepSenderName
    , "user_id"      .= wepUserId
    , "channel_name" .= wepChannelDisplayName
    , "post"         .= toValueString wepPost
    ]

--

data WEBroadcast = WEBroadcast
  { webChannelId :: Maybe ChannelId
  , webUserId    :: Maybe UserId
  , webTeamId    :: Maybe TeamId
  } deriving (Read, Show, Eq)

instance FromJSON WEBroadcast where
  parseJSON = A.withObject "WebSocketEvent Broadcast" $ \o -> do
    webChannelId          <- o .:? "channel_id"
    webTeamId             <- o .:? "team_id"
    webUserId             <- o .:? "user_id"
    return WEBroadcast { .. }

instance ToJSON WEBroadcast where
  toJSON WEBroadcast { .. } = A.object
    [ "channel_id"   .= webChannelId
    , "team_id"      .= webTeamId
    , "user_id"      .= webUserId
    ]
