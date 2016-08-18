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
, User(..)
, UserId
, Team(..)
, Channel(..)
, Channels(..)
, UserProfile(..)
, Post(..)
, Posts(..)
-- Typeclasses
, HasId(..)
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
, mmPost
, mkPendingPost
, hoistE
, noteE
, assertE
) where

import           Text.Printf ( printf )
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Network.Connection ( Connection
                                    , connectionGetLine
                                    , connectionPut
                                    , connectionClose )
import           Network.HTTP.Headers ( HeaderName(..)
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
import           Data.HashMap.Strict ( HashMap )
import           Data.Aeson ( Value
                            , ToJSON
                            , FromJSON
                            , encode
                            , eitherDecode
                            )
import           Control.Arrow ( left )

import           Network.Mattermost.Exceptions
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
mmPath :: String -> IO URI
mmPath str =
  noteE (parseRelativeReference str)
        (URIParseException ("mmPath: " ++ str))

-- | Parse the JSON body out of a request, failing if it isn't an
--   'application/json' response, or if the parsing failed
mmGetJSONBody :: FromJSON t => Response_String -> IO t
mmGetJSONBody rsp = do
  contentType <- mmGetHeader rsp HdrContentType
  assertE (contentType ~= "application/json")
          (ContentTypeException
            "mmGetJSONBody: Expected content type 'application/json'")

  -- XXX: Good for seeing the json wireformat that mattermost uses
  -- putStrLn (rspBody rsp)
  let value = left (\s -> JSONDecodeException ("mmGetJSONBody: " ++ s)
                                              (rspBody rsp))
                   (eitherDecode (BL.pack (rspBody rsp)))
  hoistE value

-- | Grab a header from the response, failing if it isn't present
mmGetHeader :: Response_String -> HeaderName -> IO String
mmGetHeader rsp hdr =
  noteE (lookupHeader hdr (rspHeaders rsp))
        (HeaderNotFoundException ("mmGetHeader: " ++ show hdr))

-- API calls

-- | We should really only need this function to get an auth token.
-- We provide it in a fairly generic form in case we need ever need it
-- but it could be inlined into mmLogin.
mmUnauthenticatedHTTPPost :: ToJSON t => ConnectionData -> URI -> t -> IO Response_String
mmUnauthenticatedHTTPPost cd path json = do
  rsp <- withConnection cd $ \con -> do
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
  hoistE $ left ConnectionException rsp

-- | Fire off a login attempt. Note: We get back more than just the auth token.
-- We also get all the server-side configuration data for the user.
mmLogin :: ConnectionData -> Login -> IO (Either LoginFailureException (Token, User))
mmLogin cd login = do
  path <- mmPath "/api/v3/users/login"

  rsp  <- mmUnauthenticatedHTTPPost cd path login
  if (rspCode rsp /= (2,0,0))
    then return (Left (LoginFailureException (show (rspCode rsp))))
    else do
      token <- mmGetHeader   rsp (HdrCustom "Token")
      value <- mmGetJSONBody rsp
      return (Right (Token token, value))

-- | Requires an authenticated user. Returns the full list of teams.
mmGetTeams :: ConnectionData -> Token -> IO (HashMap TeamId Team)
mmGetTeams cd token = mmDoRequest cd token "/api/v3/teams/all"

-- | Requires an authenticated user. Returns the full list of channels for a given team
mmGetChannels :: ConnectionData -> Token -> TeamId -> IO Channels
mmGetChannels cd token teamid = mmDoRequest cd token $
  printf "/api/v3/teams/%s/channels/" (idString teamid)

-- | Requires an authenticated user. Returns the details of a
-- specific channel.
mmGetChannel :: ConnectionData -> Token
             -> TeamId
             -> ChannelId
             -> IO Channel
mmGetChannel cd token teamid chanid = mmWithRequest cd token
  (printf "/api/v3/teams/%s/channels/%s/"
          (idString teamid)
          (idString chanid))
  (\(SC channel) -> return channel)

mmGetPosts :: ConnectionData -> Token
           -> TeamId
           -> ChannelId
           -> Int -- offset in the backlog, 0 is most recent
           -> Int -- try to fetch this many
           -> IO Posts
mmGetPosts cd token teamid chanid offset limit = mmDoRequest cd token $
  printf "/api/v3/teams/%s/channels/%s/posts/page/%d/%d"
         (idString teamid)
         (idString chanid)
         offset
         limit

mmGetUser :: ConnectionData -> Token -> UserId -> IO User
mmGetUser cd token userid = mmDoRequest cd token $
  printf "/api/v3/users/%s/get" (idString userid)

mmGetTeamMembers :: ConnectionData -> Token -> TeamId -> IO Value
mmGetTeamMembers cd token teamid = mmDoRequest cd token $
  printf "/api/v3/teams/members/%s" (idString teamid)

mmGetMe :: ConnectionData -> Token -> IO Value
mmGetMe cd token = mmDoRequest cd token "/api/v3/users/me"

mmGetProfiles :: ConnectionData -> Token
              -> TeamId -> IO (HashMap UserId UserProfile)
mmGetProfiles cd token teamid = mmDoRequest cd token $
  printf "/api/v3/users/profiles/%s" (idString teamid)

mmPost :: ConnectionData
       -> Token
       -> TeamId
       -> PendingPost
       -> IO Post -- TODO: return something informative for failures
mmPost cd token teamid post = do
  let chanid = pendingPostChannelId post
      path   = printf "/api/v3/teams/%s/channels/%s/posts/create"
                      (idString teamid)
                      (idString chanid)
  uri <- mmPath path
  rsp <- mmPOST cd token uri post
  mmGetJSONBody rsp

-- | This is for making a generic authenticated request.
mmRequest :: ConnectionData -> Token -> URI -> IO Response_String
mmRequest cd token path = do
  rawRsp <- withConnection cd $ \con -> do
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
  rsp <- hoistE $ left ConnectionException rawRsp
  assertE (rspCode rsp == (2,0,0))
          (HTTPResponseException
            ("mmRequest: expected 200 response but got: " ++ (show (rspCode rsp))))
  return rsp

-- This captures the most common pattern when making requests.
mmDoRequest :: FromJSON t
            => ConnectionData
            -> Token
            -> String
            -> IO t
mmDoRequest cd token path = mmWithRequest cd token path return

-- The slightly more general variant
mmWithRequest :: FromJSON t
              => ConnectionData
              -> Token
              -> String
              -> (t -> IO a)
              -> IO a
mmWithRequest cd token path action = do
  uri  <- mmPath path
  rsp  <- mmRequest cd token uri
  json <- mmGetJSONBody rsp
  action json

mmPOST :: ToJSON t => ConnectionData -> Token -> URI -> t -> IO Response_String
mmPOST cd token path json = do
  rawRsp <- withConnection cd $ \con -> do
    let content       = BL.toStrict (encode json)
        contentLength = B.length content
        request       = Request
          { rqURI     = path
          , rqMethod  = POST
          , rqHeaders = [ mkHeader HdrAuthorization ("Bearer " ++ getTokenString token)
                        , mkHeader HdrHost          (cdHostname cd)
                        , mkHeader HdrUserAgent     defaultUserAgent
                        , mkHeader HdrContentType   "application/json"
                        , mkHeader HdrContentLength (show contentLength)
                        ] ++ autoCloseToHeader (cdAutoClose cd)
          , rqBody    = B.unpack content
          }
    simpleHTTP_ con request
  rsp <- hoistE $ left ConnectionException rawRsp
  assertE (rspCode rsp == (2,0,0))
          (HTTPResponseException
            ("mmRequest: expected 200 response but got: " ++ (show (rspCode rsp))))
  return rsp
