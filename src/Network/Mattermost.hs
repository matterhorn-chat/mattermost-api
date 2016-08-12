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
, ChannelList(..)
-- Functions
, mkConnectionData
, mmLogin
, mmGetTeams
, mmGetChannels
, mmGetChannel
, runMM
, io
) where

import           Text.Printf ( printf )
import           Data.Default ( def )
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
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
                            , encode
                            , decode
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

-- | Return headers and a parsed JSON body from a request
mmGetJSONAndHeaders :: FromJSON t => Response_String -> MM ([Header], t)
mmGetJSONAndHeaders rsp = do
  value <- mmGetJSONBody rsp
  return (rspHeaders rsp, value)

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
mmLogin :: Login -> MM (Token, Maybe Value)
mmLogin login = do
  -- this shouldn't fail, but just for good measure
  path <- mmPath "/api/v3/users/login"

  rsp  <- mmUnauthenticatedHTTPPost path login
  assert "expected 200 response" (rspCode rsp == (2,0,0))

  token <- mmGetHeader rsp (HdrCustom "Token")
  value <- mmGetJSONBody rsp

  setToken (Token token)

  return (Token token, value)

-- | Requires an authenticated user. Returns the full list of teams.
mmGetTeams :: MM ([Header], Maybe Value)
mmGetTeams = do
  path <- mmPath "/api/v3/teams/all"
  rsp  <- mmRequest path
  mmGetJSONAndHeaders rsp

-- | Requires an authenticated user. Returns the full list of channels for a given team
mmGetChannels :: Team -> MM ([Header], Maybe Value)
mmGetChannels team = do
  path <- mmPath $ printf "/api/v3/teams/%s/channels/"
                          (getId team)
  rsp  <- mmRequest path
  mmGetJSONAndHeaders rsp

-- | Requires an authenticated user. Returns the details of a
-- specific channel.
mmGetChannel :: Team -> Channel -> MM ([Header], Maybe Value)
mmGetChannel team chan = do
  path <- mmPath $ printf "/api/v3/teams/%s/channels/%s/"
                          (getId team)
                          (getId chan)
  rsp  <- mmRequest path
  mmGetJSONAndHeaders rsp

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
