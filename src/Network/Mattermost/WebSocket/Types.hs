{-# LANGUAGE OverloadedStrings #-}

module Network.Mattermost.WebSocket.Types where

import           Data.Aeson (Object, FromJSON(..), ToJSON(..), (.:), (.=))
import qualified Data.Aeson as A
import           Data.Text (Text)

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
    "typing"             -> pure WMTyping
    "posted"             -> pure WMPosted
    "post_edited"        -> pure WMPostEdited
    "post_deleted"       -> pure WMPostDeleted
    "channel_deleted"    -> pure WMChannelDeleted
    "channel_viewed"     -> pure WMChannelViewed
    "direct_added"       -> pure WMDirectAdded
    "new_user"           -> pure WMNewUser
    "leave_team"         -> pure WMLeaveTeam
    "user_added"         -> pure WMUserAdded
    "user_removed"       -> pure WMUserRemoved
    "preference_changed" -> pure WMPreferenceChanged
    "ephemeral_message"  -> pure WMEphemeralMessage
    "status_change"      -> pure WMStatusChange
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

data WebsocketEvent = WebsocketEvent
  { weEvent     :: WebsocketEventType
  , weTeamId    :: Text
  , weChannelId :: Text
  , weUserId    :: Text
  , weData      :: Object
  } deriving (Read, Show, Eq)

instance FromJSON WebsocketEvent where
  parseJSON = A.withObject "websocket event" $ \o ->
    WebsocketEvent <$> o .: "event"
                   <*> o .: "team_id"
                   <*> o .: "channel_id"
                   <*> o .: "user_id"
                   <*> o .: "data"

instance ToJSON WebsocketEvent where
  toJSON we = A.object
    [ "event"      .= weEvent we
    , "team_id"    .= weTeamId we
    , "channel_id" .= weChannelId we
    , "user_id"    .= weUserId we
    , "data"       .= weData we
    ]

--

data WebsocketResponse = WebsocketResponse
  { wrStatus   :: Text
  , wrSeqReply :: Maybe Int
  , wrData     :: Maybe Object
  , wrError    :: Maybe Text
  } deriving (Read, Show, Eq)

instance FromJSON WebsocketResponse where
  parseJSON = A.withObject "websocket response" $ \o ->
    WebsocketResponse <$> o .: "status"
                      <*> o .: "seq_reply"
                      <*> o .: "data"
                      <*> o .: "error"

instance ToJSON WebsocketResponse where
  toJSON wr = A.object
    [ "status"    .= wrStatus wr
    , "seq_reply" .= wrSeqReply wr
    , "data"      .= wrData wr
    , "error"     .= wrError wr
    ]

--

data WebsocketRequest = WebsocketRequest
  { wqSeq    :: Int
  , wqAction :: Text
  , wqData   :: Object
  } deriving (Read, Show, Eq)

instance ToJSON WebsocketRequest where
  toJSON wq = A.object
    [ "seq"    .= wqSeq wq
    , "action" .= wqAction wq
    , "data"   .= wqData wq
    ]

instance FromJSON WebsocketRequest where
  parseJSON = A.withObject "websocket request" $ \o ->
    WebsocketRequest <$> o .: "seq"
                     <*> o .: "action"
                     <*> o .: "data"
