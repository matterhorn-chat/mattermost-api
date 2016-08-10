{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Mattermost
( -- Types
  Login(..)
, Token
, Hostname
, Port
, ConnectionData
-- Functions
, mkConnectionData
, mmLogin
, mmGetTeams
) where

import           Data.Default ( def )
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import           Network.Connection ( Connection
                                    , ConnectionParams(..)
                                    , ConnectionContext
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
import           Network.Stream as NS ( Result, Stream(..) )
import           Network.URI ( URI, parseRelativeReference )
import           System.Exit ( exitFailure )
import           Network.HTTP.Stream ( simpleHTTP_ )
import qualified Data.Aeson as A
import           Control.Exception ( bracket )

data Login
  = Login
  { username :: T.Text
  , teamname :: T.Text
  , password :: T.Text
  }

newtype Token = Token String
  deriving (Read, Show, Eq, Ord)

getTokenString :: Token -> String
getTokenString (Token s) = s

-- XXX: What value should we really use here?
maxLineLength :: Int
maxLineLength = 2^(16::Int) -- ugh, this silences a warning about defaulting

-- | This instance allows us to use 'simpleHTTP' from 'Network.HTTP.Stream' with
-- connections from the 'connection' package.
instance Stream Connection where
  readLine   con       = Right . B.unpack . dropTrailingChar <$> connectionGetLine maxLineLength con
  readBlock  con n     = Right . B.unpack <$> connectionGet con n
  writeBlock con block = Right <$> connectionPut con (B.pack block)
  close      con       = connectionClose con
  closeOnEnd _   _     = return ()

instance A.ToJSON Login where
  toJSON l = A.object ["name"     A..= teamname l
                      ,"login_id" A..= username l
                      ,"password" A..= password l
                      ]

type Hostname = String
type Port     = Int

-- For now we don't support or expose the ability to reuse connections,
-- but we have this field in case we want to support that in the future.
-- Doing so will require some modifications to withConnection (and uses).
-- Note: don't export this until we support connection reuse.
data AutoClose = No | Yes
  deriving (Read, Show, Eq, Ord)

data ConnectionData
  = ConnectionData
  { cdHostname      :: Hostname
  , cdPort          :: Port
  , cdAutoClose     :: AutoClose
  , cdConnectionCtx :: ConnectionContext
  }

mkConnectionData :: Hostname -> Port -> ConnectionContext -> ConnectionData
mkConnectionData host port ctx = ConnectionData
  { cdHostname      = host
  , cdPort          = port
  , cdConnectionCtx = ctx
  , cdAutoClose     = Yes
  }

-- | We return a list of headers so that we can treat
-- the headers like a monoid.
autoCloseToHeader :: AutoClose -> [Header]
autoCloseToHeader No  = []
autoCloseToHeader Yes = [mkHeader HdrConnection "Close"]

-- API calls

-- | We should really only need this function to get an auth token.
-- We provide it in a fairly generic form in case we need ever need it
-- but it could be inlined into mmLogin.
mmUnauthenticatedHTTPPost :: ConnectionData -> URI -> A.Value -> IO (NS.Result Response_String)
mmUnauthenticatedHTTPPost cd path json = do
  withConnection cd $ \con -> do
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
mmLogin :: ConnectionData -> Login -> IO (Token, Maybe A.Value)
mmLogin cd login = do
  let Just path = parseRelativeReference "/api/v3/users/login"
      json      = A.toJSON login
  r <- mmUnauthenticatedHTTPPost cd path json
  case r of
    Left err -> do
      print err
      exitFailure
    Right resp | rspCode resp == (2,0,0) -> do
      let hdrs = rspHeaders resp
          body = rspBody    resp

      token <- case lookupHeader (HdrCustom "Token") hdrs of
        Nothing -> do
          putStrLn "No Token Header found in auth response"
          -- XXX: Do something better than just bailing here
          exitFailure
        Just t -> return (Token t)
      value <- case lookupHeader HdrContentType hdrs of
        Just ct | ct == "application/json" -> return (A.decode (BL.pack body))
        _ -> return Nothing
      return (token, value)
    _ -> do
      putStrLn "unknown error in mmLogin"
      exitFailure

-- | Requires an authenticated user. Returns the full list of teams.
mmGetTeams :: ConnectionData -> Token -> IO ([Header], Maybe A.Value)
mmGetTeams cd token = do
  let Just path = parseRelativeReference "/api/v3/teams/all"
  r <- mmRequest cd token path
  case r of
    Left err -> do
      print err
      exitFailure
    Right rsp -> do
      let hdrs = rspHeaders rsp
          body = rspBody    rsp
      value <- case lookupHeader HdrContentType hdrs of
        Just ct | ct == "application/json" -> return (A.decode (BL.pack body))
        _ -> return Nothing
      return (hdrs, value)

-- | This is for making a generic authenticated request.
mmRequest :: ConnectionData -> Token -> URI -> IO (NS.Result Response_String)
mmRequest cd token path = do
  withConnection cd $ \con -> do
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
