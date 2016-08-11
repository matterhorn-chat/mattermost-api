{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Mattermost
( -- Types
  Login(..)
, Token
, Hostname
, Port
, ConnectionData
, Team(..)
, TeamList(..)
-- Functions
, mkConnectionData
, mmLogin
, mmGetTeams
, mmGetChannels
) where

import           Data.Default ( def )
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import           Network.Connection ( Connection
                                    , ConnectionParams(..)
                                    , connectTo
                                    , connectionGetLine
                                    , connectionGet
                                    , connectionPut
                                    , connectionClose )
import           Network.HTTP.Headers ( Header(..), HeaderName(..)
                                      , mkHeader
                                      , lookupHeader )
import           Network.HTTP.Base ( Request(..)
                                   , RequestMethod(..)
                                   , defaultUserAgent
                                   , Response_String
                                   , Response(..) )
import           Network.Stream as NS ( Stream(..) )
import           Network.URI ( URI, parseRelativeReference )
import           Network.HTTP.Stream ( simpleHTTP_ )
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import           Control.Exception ( bracket )
import           Data.Time.Clock ( UTCTime )
import           Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import qualified Data.HashMap.Strict as HM
import           Data.Ratio ( (%) )

import Network.Mattermost.Util
import Network.Mattermost.Types

data Login
  = Login
  { username :: T.Text
  , teamname :: T.Text
  , password :: T.Text
  }

--newtype Token = Token String
--  deriving (Read, Show, Eq, Ord)

--getTokenString :: Token -> String
--getTokenString (Token s) = s

-- XXX: What value should we really use here?
maxLineLength :: Int
maxLineLength = 2^(16::Int) -- ugh, this silences a warning about defaulting

-- | This instance allows us to use 'simpleHTTP' from 'Network.HTTP.Stream' with
-- connections from the 'connection' package.
instance Stream Connection where
  readLine   con       = Right . B.unpack . dropTrailingChar <$> connectionGetLine maxLineLength con
  readBlock  con n     = Right . B.unpack <$> connectionGetExact con n
  writeBlock con block = Right <$> connectionPut con (B.pack block)
  close      con       = connectionClose con
  closeOnEnd _   _     = return ()

instance A.ToJSON Login where
  toJSON l = A.object ["name"     A..= teamname l
                      ,"login_id" A..= username l
                      ,"password" A..= password l
                      ]

-- API calls

-- | We should really only need this function to get an auth token.
-- We provide it in a fairly generic form in case we need ever need it
-- but it could be inlined into mmLogin.
mmUnauthenticatedHTTPPost :: URI -> A.Value -> MM Response_String
mmUnauthenticatedHTTPPost path json = do
  cd <- getConnectionData
  ioS show $ withConnection cd $ \con -> do
    let content       = BL.toStrict (A.encode json)
        contentLength = B.length content
        request       = Request { rqURI     = path
                                , rqMethod  = POST
                                , rqHeaders = [ mkHeader HdrHost          (cdHostname cd)
                                              , mkHeader HdrUserAgent     defaultUserAgent
                                              , mkHeader HdrContentType   "application/json"
                                              , mkHeader HdrContentLength (show contentLength)
                                              ] ++ autoCloseToHeader (cdAutoClose cd)
                                , rqBody = B.unpack content
                                }
    simpleHTTP_ con request

-- | Fire off a login attempt. Note: We get back more than just the auth token.
-- We also get all the server-side configuration data for the user.
mmLogin :: Login -> MM (Token, Maybe A.Value)
mmLogin login = do
  -- this shouldn't fail, but just for good measure
  path <- noteT "error parsing path" $
            parseRelativeReference "/api/v3/users/login"

  resp <- mmUnauthenticatedHTTPPost path (A.toJSON login)
  assert "expected 200 response" $
    rspCode resp == (2,0,0)

  let hdrs = rspHeaders resp
      body = rspBody    resp

  -- make sure the headers are what we expect
  token <- noteT "No token header found in auth response"
             (lookupHeader (HdrCustom "Token") hdrs)
  contentType <- noteT "No content-type header found"
                   (lookupHeader HdrContentType hdrs)
  assert "expected JSON content type" (contentType == "application/json")

  value <- noteT "Unable to parse JSON payload" $
             A.decode (BL.pack body)

  return (Token token, value)

-- | XXX: No idea what this is
data TeamType = O | Unknown
  deriving (Read, Show, Ord, Eq)

instance A.FromJSON TeamType where
  parseJSON (A.String "O") = pure O
  parseJSON _              = pure Unknown

newtype Id = Id T.Text
  deriving (Read, Show, Eq, Ord)

instance A.FromJSON Id where
  parseJSON = A.withText "Id" $ \s ->
    pure (Id s)

getTeamIdString :: Team -> String
getTeamIdString team = case teamId team of
  Id s -> T.unpack s

data Team
  = Team
  { teamId              :: Id
  , teamCreateAt        :: UTCTime
  , teamUpdateAt        :: UTCTime
  , teamDeleteAt        :: UTCTime
  , teamDisplayName     :: String
  , teamName            :: String
  , teamEmail           :: String
  , teamType            :: TeamType
  , teamCompanyName     :: String
  , teamAllowedDomains  :: String
  , teamInviteId        :: Id
  , teamAllowOpenInvite :: Bool
  }
  deriving (Read, Show, Eq, Ord)

instance A.FromJSON Team where
  parseJSON = A.withObject "Team" $ \v -> Team     <$>
    v A..: "id"                                    <*>
    (millisecondsToUTCTime <$> v A..: "create_at") <*>
    (millisecondsToUTCTime <$> v A..: "update_at") <*>
    (millisecondsToUTCTime <$> v A..: "delete_at") <*>
    v A..: "display_name"                          <*>
    v A..: "name"                                  <*>
    v A..: "email"                                 <*>
    v A..: "type"                                  <*>
    v A..: "company_name"                          <*>
    v A..: "allowed_domains"                       <*>
    v A..: "invite_id"                             <*>
    v A..: "allow_open_invite"

newtype TeamList = TL [Team]
  deriving (Read, Show, Eq, Ord)

instance A.FromJSON TeamList where
  parseJSON = A.withObject "TeamList" $ \hm -> do
    let tl = map snd (HM.toList hm)
    tl' <- mapM A.parseJSON tl
    return (TL tl')

mmPath :: String -> MM URI
mmPath str = noteT "error parsing path" (parseRelativeReference str)

-- | Requires an authenticated user. Returns the full list of teams.
mmGetTeams :: MM ([Header], Maybe A.Value)
mmGetTeams = do
  path <- mmPath "/api/v3/teams/all"
  rsp <- mmRequest path
  mmGetJSONAndHeaders rsp

-- | Requires an authenticated user. Returns the full list of channels for a given team
mmGetChannels :: Team -> MM ([Header], Maybe A.Value)
mmGetChannels team = do
  path <- mmPath ("/api/v3/teams/" ++ getTeamIdString team ++ "/channels/")
  rsp <- mmRequest path
  mmGetJSONAndHeaders rsp

mmGetJSONAndHeaders :: A.FromJSON t => Response_String -> MM ([Header], t)
mmGetJSONAndHeaders rsp = do
  value <- mmGetJSONBody rsp
  return (rspHeaders rsp, value)

mmGetJSONBody :: A.FromJSON t => Response_String -> MM t
mmGetJSONBody rsp = do
  contentType <- noteT "No content-type header found" $
                   lookupHeader HdrContentType (rspHeaders rsp)
  assert "Expected content type 'application/json'" $
    contentType == "application/json"

  noteT "Unable to parse JSON" $ A.decode (BL.pack (rspBody rsp))

-- | This is for making a generic authenticated request.
mmRequest :: URI -> MM Response_String
mmRequest path = do
  cd <- getConnectionData
  token <- getToken
  ioS show $ withConnection cd $ \con -> do
    let request = Request { rqURI     = path
                          , rqMethod  = GET
                          , rqHeaders = [ mkHeader HdrAuthorization ("Bearer " ++ getTokenString token)
                                        , mkHeader HdrHost          (cdHostname cd)
                                        , mkHeader HdrUserAgent     defaultUserAgent
                                        ] ++ autoCloseToHeader (cdAutoClose cd)
                          , rqBody    = ""
                          }
    simpleHTTP_ con request

-- Utility code

-- | HTTP ends newlines with \r\n sequence, but the 'connection' package doesn't
-- know this so we need to drop the \r after reading lines. This should only be
-- needed in your compatibility with the HTTP library.
dropTrailingChar :: B.ByteString -> B.ByteString
dropTrailingChar bs | not (B.null bs) = B.init bs
dropTrailingChar _ = ""

-- | Creates a new connection to 'Hostname' from an already initialized 'ConnectionContext'.
-- Internally it uses 'bracket' to cleanup the connection.
withConnection :: ConnectionData -> (Connection -> IO a) -> IO a
withConnection cd action =
  bracket (connectTo (cdConnectionCtx cd) $ ConnectionParams
            { connectionHostname  = cdHostname cd
            , connectionPort      = fromIntegral (cdPort cd)
            , connectionUseSecure = Just def
            , connectionUseSocks  = Nothing
            }
          )
          connectionClose
          action

millisecondsToUTCTime :: Integer -> UTCTime
millisecondsToUTCTime ms = posixSecondsToUTCTime (fromRational (ms%1000))

-- | Get exact count of bytes from a connection.
--
-- The size argument is the exact amount that must be returned to the user.
-- The call will wait until all data is available.  Hence, it behaves like
-- 'B.hGet'.
--
-- On end of input, 'connectionGetExact' will throw an 'E.isEOFError'
-- exception.
-- Taken from: https://github.com/vincenthz/hs-connection/issues/9
connectionGetExact :: Connection -> Int -> IO B.ByteString
connectionGetExact con n = loop B.empty 0
  where loop bs y
          | y == n = return bs
          | otherwise = do
            next <- connectionGet con (n - y)
            loop (B.append bs next) (y + (B.length next))
