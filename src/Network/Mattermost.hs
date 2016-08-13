{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Mattermost
( -- Types
  Login(..)
, Token
, Hostname
, Port
, ConnectionData
, Id(..)
, Team(..)
, TeamList(..)
, Channel(..)
, ChannelList(..)
, UserProfile(..)
, Post(..)
, Posts(..)
, MMResult(..)
-- Functions
, mkConnectionData
, mmLogin
, mmGetTeams
, mmGetChannels
, mmGetChannel
, mmGetPosts
, mmGetUser
, mmGetTeamMembers
, mmGetMe
, mmGetProfiles
, runMM
, io
) where

import           Text.Printf ( printf )
import           Data.Default ( def )
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.HashMap.Strict (HashMap)
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
import           Data.Aeson ( Value
                            , ToJSON
                            , FromJSON
                            , Result(..)
                            , encode
                            , decode
                            , fromJSON
                            )
import           Control.Exception ( bracket )

import           Network.Mattermost.Util
import           Network.Mattermost.Types

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


-- MM utility functions

-- | Parse a path, failing if we cannot.
mmPath :: String -> MM URI
mmPath str = noteT "error parsing path" (parseRelativeReference str)

-- | This wraps up a payload for us:
data MMResult a = MMResult
  { mmPayload  :: a
  , mmJSONBody :: Value
  , mmHeaders  :: [Header]
  } deriving (Show)

instance Functor MMResult where
  fmap f mm = MMResult
    { mmPayload  = f (mmPayload mm)
    , mmJSONBody = mmJSONBody mm
    , mmHeaders  = mmHeaders mm
    }

-- | Return headers and a parsed JSON body from a request
mmGetResult :: FromJSON t => Response_String -> MM (MMResult t)
mmGetResult rsp = do
  value <- mmGetJSONBody rsp
  decoded <- case fromJSON value of
              Success t -> pure t
              Error s   -> fail s
  return (MMResult decoded value (rspHeaders rsp))

-- | Parse the JSON body out of a request, failing if it isn't an
--   'application/json' response, or if the parsing failed
mmGetJSONBody :: FromJSON t => Response_String -> MM t
mmGetJSONBody rsp = do
  contentType <- mmGetHeader rsp HdrContentType
  assert "Expected content type 'application/json'" $
    contentType ~= "application/json"

  noteT "Unable to parse JSON" $ decode (BL.pack (rspBody rsp))

-- | Grab a header from the response, failing if it isn't present
mmGetHeader :: Response_String -> HeaderName -> MM String
mmGetHeader rsp hdr = do
  noteT ("Cannot find header " ++ show hdr) $
    lookupHeader hdr (rspHeaders rsp)


-- API calls

-- | We should really only need this function to get an auth token.
-- We provide it in a fairly generic form in case we need ever need it
-- but it could be inlined into mmLogin.
mmUnauthenticatedHTTPPost :: ToJSON t => URI -> t -> MM Response_String
mmUnauthenticatedHTTPPost path json = do
  cd <- getConnectionData
  ioS show $ withConnection cd $ \con -> do
    let content       = BL.toStrict (encode json)
        contentLength = B.length content
        request       = Request
          { rqURI     = path
          , rqMethod  = POST
          , rqHeaders = [ mkHeader HdrHost          (cdHostname cd)
                        , mkHeader HdrUserAgent     defaultUserAgent
                        , mkHeader HdrContentType   "application/json"
                        , mkHeader HdrContentLength (show contentLength)
                        ] ++ autoCloseToHeader (cdAutoClose cd)
          , rqBody    = B.unpack content
          }
    simpleHTTP_ con request

-- | Fire off a login attempt. Note: We get back more than just the auth token.
-- We also get all the server-side configuration data for the user.
mmLogin :: Login -> MM (MMResult Token)
mmLogin login = do
  -- this shouldn't fail, but just for good measure
  path <- mmPath "/api/v3/users/login"

  rsp  <- mmUnauthenticatedHTTPPost path login
  assert "expected 200 response" (rspCode rsp == (2,0,0))

  token <- mmGetHeader rsp (HdrCustom "Token")
  value <- mmGetJSONBody rsp

  setToken (Token token)

  return MMResult
    { mmPayload = Token token
    , mmJSONBody = value
    , mmHeaders = rspHeaders rsp
    }

-- | Requires an authenticated user. Returns the full list of teams.
mmGetTeams :: MM (MMResult TeamList)
mmGetTeams = do
  path <- mmPath "/api/v3/teams/all"
  rsp  <- mmRequest path
  mmGetResult rsp

-- | Requires an authenticated user. Returns the full list of channels for a given team
mmGetChannels :: Team -> MM (MMResult ChannelList)
mmGetChannels team = do
  path <- mmPath $ printf "/api/v3/teams/%s/channels/"
                          (getId team)
  rsp  <- mmRequest path
  mmGetResult rsp

-- | Requires an authenticated user. Returns the details of a
-- specific channel.
mmGetChannel :: Team -> Channel -> MM (MMResult Value)
mmGetChannel team chan = do
  path <- mmPath $ printf "/api/v3/teams/%s/channels/%s/"
                          (getId team)
                          (getId chan)
  rsp  <- mmRequest path
  mmGetResult rsp

mmGetPosts :: Team -> Channel
           -> Int -- offset in the backlog, 0 is most recent
           -> Int -- try to fetch this many
           -> MM (MMResult Posts)
mmGetPosts team chan offset limit = do
  path <- mmPath $ printf "/api/v3/teams/%s/channels/%s/posts/page/%d/%d"
                          (getId team)
                          (getId chan)
                          offset
                          limit
  rsp  <- mmRequest path
  mmGetResult rsp

mmGetUser :: UserProfile -> MM (MMResult Value)
mmGetUser user = do
  path <- mmPath $ printf "/api/v3/users/%s/get"
                          (getId user)
  rsp  <- mmRequest path
  mmGetResult rsp

mmGetTeamMembers :: Team -> MM (MMResult Value)
mmGetTeamMembers team = do
  path <- mmPath $ printf "/api/v3/teams/members/%s"
                          (getId team)
  rsp  <- mmRequest path
  mmGetResult rsp

mmGetMe :: MM (MMResult Value)
mmGetMe = do
  path <- mmPath "/api/v3/users/me"
  rsp  <- mmRequest path
  mmGetResult rsp

mmGetProfiles :: Team -> MM (MMResult (HashMap Id UserProfile))
mmGetProfiles team = do
  path <- mmPath $ printf "/api/v3/users/profiles/%s"
                          (getId team)
  rsp  <- mmRequest path
  mmGetResult rsp

-- | This is for making a generic authenticated request.
mmRequest :: URI -> MM Response_String
mmRequest path = do
  cd    <- getConnectionData
  token <- getToken
  ioS show $ withConnection cd $ \con -> do
    let request = Request
          { rqURI     = path
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
