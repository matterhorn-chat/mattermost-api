{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Mattermost.Lenses where

import Network.Mattermost.Types
import Network.Mattermost.WebSocket.Types
import Network.Mattermost.TH

-- | This is the same type alias as in @Control.Lens@, and so can be used
-- anywhere lenses are needed.
type Lens' a b = forall f. Functor f => (b -> f b) -> (a -> f a)

-- * 'ConnectionData' lenses
suffixLenses ''ConnectionData

-- * 'Login' lenses
suffixLenses ''Login

-- * 'Team' lenses
suffixLenses ''Team

-- * 'TeamMember' lenses
suffixLenses ''TeamMember

-- * 'NotifyProps' lenses
suffixLenses ''NotifyProps

-- * 'Channel' lenses
suffixLenses ''Channel

-- * 'ChannelData' lenses
suffixLenses ''ChannelData

-- * 'User' lenses
suffixLenses ''User

-- * 'Post' lenses
suffixLenses ''Post

-- * 'PostProps' lenses
suffixLenses ''PostProps
suffixLenses ''PostPropAttachment

-- * 'PendingPost' lenses
suffixLenses ''PendingPost

-- * 'Posts' lenses
suffixLenses ''Posts

-- * 'Reaction' lenses
suffixLenses ''Reaction

-- * 'WebsocketEvent' lenses
suffixLenses ''WebsocketEvent

-- * 'WEData' lenses
suffixLenses ''WEData

-- * 'WEBroadcast' lenses
suffixLenses ''WEBroadcast

-- * 'CommandResponse' lenses
suffixLenses ''CommandResponse
suffixLenses ''CommandResponseType
