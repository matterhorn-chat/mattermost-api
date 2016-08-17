module Network.Mattermost.Exceptions
( -- Exception Types
  LoginFailureException(..)
, URIParseException(..)
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

-- Unlike many exceptions in this file, this is a mattermost specific exception
data LoginFailureException = LoginFailureException String
  deriving (Show, Typeable)

instance Exception LoginFailureException

--

-- TODO: These three (URIParseException, ContentTypeException, and
-- JSONDecodeException)
-- are good candidates to be merged into a sum type

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

-- TODO: combine these three into one type

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
