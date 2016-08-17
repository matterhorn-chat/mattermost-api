module Network.Mattermost.Exceptions
( -- Exception Types
  URIParseException(..)
, ContentTypeException(..)
, JSONDecodeException(..)
, HeaderNotFoundException(..)
, HTTPResponseException(..)
, ConnectionException(..)
) where

import           Data.Typeable ( Typeable )
import           Control.Exception ( Exception(..) )
import           Network.Stream ( ConnError )

--

data URIParseException = URIParseException String
  deriving (Show, Typeable)

instance Exception URIParseException

--

data ContentTypeException = ContentTypeException String
  deriving (Show, Typeable)

instance Exception ContentTypeException

--

data JSONDecodeException
  = JSONDecodeException
  { jsonDecodeExceptionMsg  :: String
  , jsonDecodeExceptionJSON :: String
  } deriving (Show, Typeable)

instance Exception JSONDecodeException

--

data HeaderNotFoundException = HeaderNotFoundException String
  deriving (Show, Typeable)

instance Exception HeaderNotFoundException

--

data HTTPResponseException = HTTPResponseException String
  deriving (Show, Typeable)

instance Exception HTTPResponseException

--

data ConnectionException = ConnectionException ConnError
 deriving (Show, Typeable)

instance Exception ConnectionException
