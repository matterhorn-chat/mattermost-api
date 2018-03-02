{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Mattermost.Exceptions
( -- Exception Types
  LoginFailureException(..)
, URIParseException(..)
, ContentTypeException(..)
, JSONDecodeException(..)
, HeaderNotFoundException(..)
, HTTPResponseException(..)
, MattermostError(..)
, ConnectionException(..)
, MattermostServerError(..)
) where

import qualified Data.Aeson as A
import qualified Data.Text as T
import           Data.Typeable ( Typeable )
import           Control.Exception ( Exception(..) )
import           Network.Stream ( ConnError )

--

-- Unlike many exceptions in this file, this is a mattermost specific exception
data LoginFailureException = LoginFailureException String
  deriving (Show, Typeable)

instance Exception LoginFailureException

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

data MattermostError = MattermostError
  { mattermostErrorId         :: T.Text
  , mattermostErrorMessage    :: T.Text
  , mattermostErrorRequestId  :: T.Text
  , mattermostErrorStatusCode :: Int
  , mattermostErrorIsOAuth    :: Bool
  } deriving (Show, Typeable)

instance Exception MattermostError

instance A.FromJSON MattermostError where
  parseJSON = A.withObject "MattermostError" $ \o -> do
    mattermostErrorId         <- o A..: "id"
    mattermostErrorMessage    <- o A..: "message"
    mattermostErrorRequestId  <- o A..: "request_id"
    mattermostErrorStatusCode <- o A..: "status_code"
    mattermostErrorIsOAuth    <- o A..:? "is_oauth" A..!= False
    return MattermostError { .. }

data MattermostServerError = MattermostServerError T.Text
  deriving (Show, Typeable)

instance Exception MattermostServerError

--

data HTTPResponseException = HTTPResponseException String
  deriving (Show, Typeable)

instance Exception HTTPResponseException

--

data ConnectionException = ConnectionException ConnError
 deriving (Show, Typeable)

instance Exception ConnectionException
