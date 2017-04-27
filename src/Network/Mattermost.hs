{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Mattermost
( -- * Types
  -- ** Mattermost-Related Types
  Login(..)
, Hostname
, Port
, ConnectionData
, Session
, Id(..)
, User(..)
, UserId(..)
, InitialLoad(..)
, Team(..)
, TeamMember(..)
, Type(..)
, TeamId(..)
, TeamsCreate(..)
, Channel(..)
, ChannelWithData(..)
, ChannelData(..)
, ChannelId(..)
, Channels
, MinChannel(..)
, UsersCreate(..)
, Post(..)
, PostProps(..)
, PendingPost(..)
, PostId(..)
, FileId(..)
, FileInfo(..)
, Reaction(..)
, urlForFile
, Posts(..)
, MinCommand(..)
, CommandResponse(..)
, CommandResponseType(..)
-- ** Log-related types
, Logger
, LogEvent(..)
, LogEventType(..)
, withLogger
, noLogger
-- * Typeclasses
, HasId(..)
-- * HTTP API Functions
, mkConnectionData
, initConnectionData
, initConnectionDataInsecure
, mmLogin
, mmCreateDirect
, mmCreateChannel
, mmCreateTeam
, mmDeleteChannel
, mmLeaveChannel
, mmJoinChannel
, mmGetTeams
, mmGetChannels
, mmGetMoreChannels
, mmGetChannel
, mmUpdateLastViewedAt
, mmDeletePost
, mmGetPost
, mmGetPosts
, mmGetPostsSince
, mmGetPostsBefore
, mmGetPostsAfter
, mmGetReactionsForPost
, mmGetFileInfo
, mmGetUser
, mmGetUsers
, mmGetTeamMembers
, mmGetChannelMembers
, mmGetProfilesForDMList
, mmGetMe
, mmGetProfiles
, mmGetStatuses
, mmGetInitialLoad
, mmSaveConfig
, mmSetChannelHeader
, mmChannelAddUser
, mmTeamAddUser
, mmUsersCreate
, mmUsersCreateWithSession
, mmPost
, mmUpdatePost
, mmExecute
, mmGetConfig
, mkPendingPost
, idString
, hoistE
, noteE
, assertE
) where

import           Control.Exception (throwIO)
import           Control.Monad (when)
import           Data.Monoid ((<>))
import           Text.Printf ( printf )
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Time.Clock ( UTCTime )
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
import qualified Data.HashMap.Strict as HM
import           Data.Aeson ( Value(..)
                            , ToJSON(..)
                            , FromJSON
                            , object
                            , (.=)
                            , encode
                            , eitherDecode
                            )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Control.Arrow ( left )

import           Network.Mattermost.Exceptions
import           Network.Mattermost.Util
import           Network.Mattermost.Types

maxLineLength :: Int
maxLineLength = 2^(16::Int)

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
mmGetJSONBody :: FromJSON t => String -> Response_String -> IO (Value, t)
mmGetJSONBody label rsp = do
  contentType <- mmGetHeader rsp HdrContentType
  assertE (contentType ~= "application/json")
          (ContentTypeException
            ("mmGetJSONBody: " ++ label ++ ": " ++
             "Expected content type 'application/json'" ++
             " found " ++ contentType))

  let value = left (\s -> JSONDecodeException ("mmGetJSONBody: " ++ label ++ ": " ++ s)
                                              (rspBody rsp))
                   (eitherDecode (BL.pack (rspBody rsp)))
  let rawVal = left (\s -> JSONDecodeException ("mmGetJSONBody: " ++ label ++ ": " ++ s)
                                              (rspBody rsp))
                   (eitherDecode (BL.pack (rspBody rsp)))
  hoistE $ do
    x <- rawVal
    y <- value
    return (x, y)

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
          , rqHeaders = [ mkHeader HdrHost          (T.unpack $ cdHostname cd)
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
mmLogin :: ConnectionData -> Login -> IO (Either LoginFailureException (Session, User))
mmLogin cd login = do
  let rawPath = "/api/v3/users/login"
  path <- mmPath rawPath
  runLogger cd "mmLogin" $
    HttpRequest GET rawPath (Just (toJSON login))
  rsp  <- mmUnauthenticatedHTTPPost cd path login
  if (rspCode rsp /= (2,0,0))
    then return (Left (LoginFailureException (show (rspCode rsp))))
    else do
      token <- mmGetHeader   rsp (HdrCustom "Token")
      (raw, value) <- mmGetJSONBody "User" rsp
      runLogger cd "mmLogin" $
        HttpResponse 200 rawPath (Just raw)
      return (Right (Session cd (Token token), value))

-- | Fire off a login attempt. Note: We get back more than just the auth token.
-- We also get all the server-side configuration data for the user.
mmGetInitialLoad :: Session -> IO InitialLoad
mmGetInitialLoad sess =
  mmDoRequest sess "mmGetInitialLoad" "/api/v3/users/initial_load"

-- | Requires an authenticated user. Returns the full list of teams.
mmGetTeams :: Session -> IO (HashMap TeamId Team)
mmGetTeams sess =
  mmDoRequest sess "mmGetTeams" "/api/v3/teams/all"

mmCreateTeam :: Session -> TeamsCreate -> IO Team
mmCreateTeam sess payload = do
  let path = "/api/v3/teams/create"
  uri <- mmPath path
  runLoggerS sess "mmCreateTeam" $
    HttpRequest POST path (Just (toJSON payload))
  rsp <- mmPOST sess uri payload
  (val, r) <- mmGetJSONBody "Team" rsp
  runLoggerS sess "mmCreateTeam" $
    HttpResponse 200 path (Just val)
  return r

-- | Requires an authenticated user. Returns the full list of channels
-- for a given team of which the user is a member
mmGetChannels :: Session -> TeamId -> IO Channels
mmGetChannels sess teamid = mmDoRequest sess "mmGetChannels" $
  printf "/api/v3/teams/%s/channels/" (idString teamid)

-- | Requires an authenticated user. Returns the channels for a team of
-- which the user is not already a member
mmGetMoreChannels :: Session -> TeamId -> IO Channels
mmGetMoreChannels sess teamid = mmDoRequest sess "mmGetMoreChannels" $
  printf "/api/v3/teams/%s/channels/more" (idString teamid)

-- | Requires an authenticated user. Returns the details of a
-- specific channel.
mmGetChannel :: Session
             -> TeamId
             -> ChannelId
             -> IO ChannelWithData
mmGetChannel sess teamid chanid = mmWithRequest sess "mmGetChannel"
  (printf "/api/v3/teams/%s/channels/%s/"
          (idString teamid)
          (idString chanid))
  return

mmUpdateLastViewedAt :: Session
                     -> TeamId
                     -> ChannelId
                     -> IO ()
mmUpdateLastViewedAt sess teamid chanid = do
  let uri = printf "/api/v3/teams/%s/channels/%s/update_last_viewed_at"
                   (idString teamid)
                   (idString chanid)
  path <- mmPath uri
  runLoggerS sess "mmUpdateLastViewedAt" $
    HttpRequest POST uri Nothing
  _ <- mmRawPOST sess path ""
  runLoggerS sess "mmUpdateLastViewedAt" $
    HttpResponse 200 uri Nothing
  return ()

mmJoinChannel :: Session
              -> TeamId
              -> ChannelId
              -> IO ()
mmJoinChannel sess teamid chanid = do
  let path = printf "/api/v3/teams/%s/channels/%s/join"
                   (idString teamid)
                   (idString chanid)
  uri <- mmPath path
  runLoggerS sess "mmJoinChannel" $
    HttpRequest POST path Nothing
  rsp <- mmPOST sess uri (""::T.Text)
  (val, (_::Channel)) <- mmGetJSONBody "Channel" rsp
  runLoggerS sess "mmJoinChannel" $
    HttpResponse 200 path (Just val)
  return ()

mmLeaveChannel :: Session
               -> TeamId
               -> ChannelId
               -> IO ()
mmLeaveChannel sess teamid chanid = do
  let path = printf "/api/v3/teams/%s/channels/%s/leave"
                   (idString teamid)
                   (idString chanid)
      payload = HM.fromList [("id" :: T.Text, chanid)]
  uri <- mmPath path
  runLoggerS sess "mmLeaveChannel" $
    HttpRequest POST path (Just (toJSON payload))
  rsp <- mmPOST sess uri payload
  (val, (_::HM.HashMap T.Text ChannelId)) <- mmGetJSONBody "Channel name/ID map" rsp
  runLoggerS sess "mmCreateDirect" $
    HttpResponse 200 path (Just val)
  return ()

mmGetPosts :: Session
           -> TeamId
           -> ChannelId
           -> Int -- offset in the backlog, 0 is most recent
           -> Int -- try to fetch this many
           -> IO Posts
mmGetPosts sess teamid chanid offset limit =
  mmDoRequest sess "mmGetPosts" $
  printf "/api/v3/teams/%s/channels/%s/posts/page/%d/%d"
         (idString teamid)
         (idString chanid)
         offset
         limit

mmGetPostsSince :: Session
           -> TeamId
           -> ChannelId
           -> UTCTime
           -> IO Posts
mmGetPostsSince sess teamid chanid since =
  mmDoRequest sess "mmGetPostsSince" $
  printf "/api/v3/teams/%s/channels/%s/posts/since/%d"
         (idString teamid)
         (idString chanid)
         (utcTimeToMilliseconds since :: Int)

mmGetPost :: Session
          -> TeamId
          -> ChannelId
          -> PostId
          -> IO Posts
mmGetPost sess teamid chanid postid = do
  let path = printf "/api/v3/teams/%s/channels/%s/posts/%s/get"
             (idString teamid)
             (idString chanid)
             (idString postid)
  uri <- mmPath path
  rsp <- mmRequest sess uri
  (raw, json) <- mmGetJSONBody "Posts" rsp
  runLoggerS sess "mmGetPost" $
    HttpResponse 200 path (Just raw)
  return json

mmGetPostsAfter :: Session
                -> TeamId
                -> ChannelId
                -> PostId
                -> Int -- offset in the backlog, 0 is most recent
                -> Int -- try to fetch this many
                -> IO Posts
mmGetPostsAfter sess teamid chanid postid offset limit =
  mmDoRequest sess "mmGetPosts" $
  printf "/api/v3/teams/%s/channels/%s/posts/%s/after/%d/%d"
         (idString teamid)
         (idString chanid)
         (idString postid)
         offset
         limit

mmGetPostsBefore :: Session
                -> TeamId
                -> ChannelId
                -> PostId
                -> Int -- offset in the backlog, 0 is most recent
                -> Int -- try to fetch this many
                -> IO Posts
mmGetPostsBefore sess teamid chanid postid offset limit =
  mmDoRequest sess "mmGetPosts" $
  printf "/api/v3/teams/%s/channels/%s/posts/%s/before/%d/%d"
         (idString teamid)
         (idString chanid)
         (idString postid)
         offset
         limit

mmGetFileInfo :: Session
              -> FileId
              -> IO FileInfo
mmGetFileInfo sess fileId =
  mmDoRequest sess "mmGetFileInfo" $
  printf "/api/v3/files/%s/get_info" (idString fileId)

mmGetUser :: Session -> UserId -> IO User
mmGetUser sess userid = mmDoRequest sess "mmGetUser" $
  printf "/api/v3/users/%s/get" (idString userid)

mmGetUsers :: Session -> Int -> Int -> IO (HashMap UserId User)
mmGetUsers sess offset limit =
  mmDoRequest sess "mmGetUsers" $
    printf "/api/v3/users/%d/%d" offset limit

mmGetTeamMembers :: Session -> TeamId -> IO (Seq.Seq TeamMember)
mmGetTeamMembers sess teamid = mmDoRequest sess "mmGetTeamMembers" $
  printf "/api/v3/teams/members/%s" (idString teamid)

mmGetChannelMembers :: Session -> TeamId -> ChannelId -> IO (HashMap UserId User)
mmGetChannelMembers sess teamid chanid = mmDoRequest sess "mmGetChannelMembers" $
  printf "/api/v3/teams/%s/channels/%s/users/%d/%d" (idString teamid) (idString chanid) (0::Int) (10000::Int)

mmGetProfilesForDMList :: Session -> TeamId
                       -> IO (HashMap UserId User)
mmGetProfilesForDMList sess teamid =
  mmDoRequest sess "mmGetProfilesForDMList" $
    printf "/api/v3/users/profiles_for_dm_list/%s" (idString teamid)

mmGetMe :: Session -> IO User
mmGetMe sess = mmDoRequest sess "mmGetMe" "/api/v3/users/me"

mmGetProfiles :: Session
              -> TeamId -> IO (HashMap UserId User)
mmGetProfiles sess teamid = mmDoRequest sess "mmGetProfiles" $
  printf "/api/v3/teams/%s/users/%d/%d" (idString teamid) (0::Int) (10000::Int)

mmGetStatuses :: Session -> IO (HashMap UserId T.Text)
mmGetStatuses sess = mmDoRequest sess "mmGetStatuses" $
  printf "/api/v3/users/status"

-- POST /api/v3/teams/{}/create_direct with {"user_id": _}
mmCreateDirect :: Session -> TeamId -> UserId -> IO Channel
mmCreateDirect sess teamid userid = do
  let path = printf "/api/v3/teams/%s/channels/create_direct" (idString teamid)
      payload = HM.fromList [("user_id" :: T.Text, userid)]
  uri <- mmPath path
  runLoggerS sess "mmCreateDirect" $
    HttpRequest POST path (Just (toJSON payload))
  rsp <- mmPOST sess uri payload
  (val, r) <- mmGetJSONBody "Channel" rsp
  runLoggerS sess "mmCreateDirect" $
    HttpResponse 200 path (Just val)
  return r

-- { name, display_name, purpose, header }
mmCreateChannel :: Session -> TeamId -> MinChannel -> IO Channel
mmCreateChannel sess teamid payload = do
  let path = printf "/api/v3/teams/%s/channels/create" (idString teamid)
  uri <- mmPath path
  runLoggerS sess "mmCreateChannel" $
    HttpRequest POST path (Just (toJSON payload))
  rsp <- mmPOST sess uri payload
  (val, r) <- mmGetJSONBody "Channel" rsp
  runLoggerS sess "mmCreateChannel" $
    HttpResponse 200 path (Just val)
  return r

mmDeleteChannel :: Session -> TeamId -> ChannelId -> IO ()
mmDeleteChannel sess teamid chanid = do
  let path = printf "/api/v3/teams/%s/channels/%s/delete"
               (idString teamid) (idString chanid)
  uri <- mmPath path
  runLoggerS sess "mmDeleteChannel" $
    HttpRequest POST path Nothing
  _ <- mmRawPOST sess uri ""
  runLoggerS sess "mmDeleteChannel" $
    HttpResponse 200 path Nothing
  return ()

mmDeletePost :: Session
             -> TeamId
             -> ChannelId
             -> PostId
             -> IO ()
mmDeletePost sess teamid chanid postid = do
  let path   = printf "/api/v3/teams/%s/channels/%s/posts/%s/delete"
                      (idString teamid)
                      (idString chanid)
                      (idString postid)
  uri <- mmPath path
  runLoggerS sess "mmDeletePost" $
    HttpRequest POST path Nothing
  rsp <- mmPOST sess uri ([]::[String])
  (_, _::Value) <- mmGetJSONBody "Post" rsp
  runLoggerS sess "mmDeletePost" $
    HttpResponse 200 path Nothing
  return ()

mmUpdatePost :: Session
             -> TeamId
             -> Post
             -> IO Post
mmUpdatePost sess teamid post = do
  let chanid = postChannelId post
      path   = printf "/api/v3/teams/%s/channels/%s/posts/update"
                      (idString teamid)
                      (idString chanid)
  uri <- mmPath path
  runLoggerS sess "mmUpdatePost" $
    HttpRequest POST path (Just (toJSON post))
  rsp <- mmPOST sess uri post
  (val, r) <- mmGetJSONBody "Post" rsp
  runLoggerS sess "mmUpdatePost" $
    HttpResponse 200 path (Just (val))
  return r

mmPost :: Session
       -> TeamId
       -> PendingPost
       -> IO Post
mmPost sess teamid post = do
  let chanid = pendingPostChannelId post
      path   = printf "/api/v3/teams/%s/channels/%s/posts/create"
                      (idString teamid)
                      (idString chanid)
  uri <- mmPath path
  runLoggerS sess "mmPost" $
    HttpRequest POST path (Just (toJSON post))
  rsp <- mmPOST sess uri post
  (val, r) <- mmGetJSONBody "Post" rsp
  runLoggerS sess "mmPost" $
    HttpResponse 200 path (Just (val))
  return r

-- | Get the system configuration. Requires administrative permission.
mmGetConfig :: Session
            -> IO Value
mmGetConfig sess =
  mmDoRequest sess "mmGetConfig" "/api/v3/admin/config"

mmSaveConfig :: Session
             -> Value
             -> IO ()
mmSaveConfig sess config = do
  let path = "/api/v3/admin/save_config"
  uri <- mmPath path
  runLoggerS sess "mmSaveConfig" $
    HttpRequest POST path (Just config)
  _ <- mmPOST sess uri config
  runLoggerS sess "mmSaveConfig" $
    HttpResponse 200 path Nothing
  return ()

mmChannelAddUser :: Session
                 -> TeamId
                 -> ChannelId
                 -> UserId
                 -> IO ChannelData
mmChannelAddUser sess teamid chanId uId = do
  let path = printf "/api/v3/teams/%s/channels/%s/add"
                    (idString teamid)
                    (idString chanId)
      req = object ["user_id" .= uId]
  uri <- mmPath path
  runLoggerS sess "mmChannelAddUser" $
    HttpRequest POST path (Just req)
  rsp <- mmPOST sess uri req
  (val, r) <- mmGetJSONBody "ChannelData" rsp
  runLoggerS sess "mmChannelAddUser" $
    HttpResponse 200 path (Just val)
  return r

mmTeamAddUser :: Session
              -> TeamId
              -> UserId
              -> IO ()
mmTeamAddUser sess teamid uId = do
  let path = printf "/api/v3/teams/%s/add_user_to_team"
                    (idString teamid)
      req  = object ["user_id" .= uId]
  uri <- mmPath path
  runLoggerS sess "mmTeamAddUser" $
    HttpRequest POST path (Just req)
  _ <- mmPOST sess uri req
  runLoggerS sess "mmTeamAddUSer" $
    HttpResponse 200 path Nothing
  return ()

mmExecute :: Session
          -> TeamId
          -> MinCommand
          -> IO CommandResponse
mmExecute sess teamid command = do
  let path   = printf "/api/v3/teams/%s/commands/execute"
                      (idString teamid)
  uri <- mmPath path
  runLoggerS sess "mmExecute" $
    HttpRequest POST path (Just (toJSON command))
  rsp <- mmPOST sess uri command
  (val, r) <- mmGetJSONBody "Value" rsp
  runLoggerS sess "mmExecute" $
    HttpResponse 200 path (Just (val))
  return r

mmUsersCreate :: ConnectionData
              -> UsersCreate
              -> IO User
mmUsersCreate cd usersCreate = do
  let path = "/api/v3/users/create"
  uri <- mmPath path
  runLogger cd "mmUsersCreate" $
    HttpRequest POST path (Just (toJSON usersCreate))
  rsp <- mmUnauthenticatedHTTPPost cd uri usersCreate
  (val, r) <- mmGetJSONBody "User" rsp
  runLogger cd "mmUsersCreate" $
    HttpResponse 200 path (Just (val))
  return r

mmUsersCreateWithSession :: Session
                         -> UsersCreate
                         -> IO User
mmUsersCreateWithSession sess usersCreate = do
  let path = "/api/v3/users/create"
  uri <- mmPath path
  runLoggerS sess "mmUsersCreateWithToken" $
    HttpRequest POST path (Just (toJSON usersCreate))
  rsp <- mmPOST sess uri usersCreate
  (val, r) <- mmGetJSONBody "User" rsp
  runLoggerS sess "mmUsersCreateWithToken" $
    HttpResponse 200 path (Just (val))
  return r

mmGetReactionsForPost :: Session
                      -> TeamId
                      -> ChannelId
                      -> PostId
                      -> IO [Reaction]
mmGetReactionsForPost sess tId cId pId = do
  let path = printf "/api/v3/teams/%s/channels/%s/posts/%s/reactions"
                    (idString tId)
                    (idString cId)
                    (idString pId)
  mmDoRequest sess "mmGetReactionsForPost" path

-- | This is for making a generic authenticated request.
mmRequest :: Session -> URI -> IO Response_String
mmRequest (Session cd token) path = do
  rawRsp <- withConnection cd $ \con -> do
    let request = Request
          { rqURI     = path
          , rqMethod  = GET
          , rqHeaders = [ mkHeader HdrAuthorization ("Bearer " ++ getTokenString token)
                        , mkHeader HdrHost          (T.unpack $ cdHostname cd)
                        , mkHeader HdrUserAgent     defaultUserAgent
                        ] ++ autoCloseToHeader (cdAutoClose cd)
          , rqBody    = ""
          }
    simpleHTTP_ con request
  rsp <- hoistE $ left ConnectionException rawRsp
  assert200Response path rsp
  return rsp

-- This captures the most common pattern when making requests.
mmDoRequest :: FromJSON t
            => Session
            -> String
            -> String
            -> IO t
mmDoRequest sess fnname path = mmWithRequest sess fnname path return

-- The slightly more general variant
mmWithRequest :: FromJSON t
              => Session
              -> String
              -> String
              -> (t -> IO a)
              -> IO a
mmWithRequest sess@(Session cd _) fnname path action = do
  uri  <- mmPath path
  runLogger cd fnname $
    HttpRequest GET path Nothing
  rsp  <- mmRequest sess uri
  (raw,json) <- mmGetJSONBody fnname rsp
  runLogger cd fnname $
    HttpResponse 200 path (Just raw)
  action json

mmPOST :: ToJSON t => Session -> URI -> t -> IO Response_String
mmPOST sess path json =
  mmRawPOST sess path (BL.toStrict (encode json))

mmSetChannelHeader :: Session -> TeamId -> ChannelId -> T.Text -> IO Channel
mmSetChannelHeader sess teamid chanid header = do
  let path = printf "/api/v3/teams/%s/channels/update_header"
                    (idString teamid)
  uri <- mmPath path
  let req = SetChannelHeader chanid header
  runLoggerS sess "mmSetChannelHeader" $
    HttpRequest POST path (Just (toJSON req))
  rsp <- mmPOST sess uri req
  (_, r) <- mmGetJSONBody "Channel" rsp
  return r

mmRawPOST :: Session -> URI -> B.ByteString -> IO Response_String
mmRawPOST (Session cd token) path content = do
  rawRsp <- withConnection cd $ \con -> do
    let contentLength = B.length content
        request       = Request
          { rqURI     = path
          , rqMethod  = POST
          , rqHeaders = [ mkHeader HdrAuthorization ("Bearer " ++ getTokenString token)
                        , mkHeader HdrHost          (T.unpack $ cdHostname cd)
                        , mkHeader HdrUserAgent     defaultUserAgent
                        , mkHeader HdrContentType   "application/json"
                        , mkHeader HdrContentLength (show contentLength)
                        ] ++ autoCloseToHeader (cdAutoClose cd)
          , rqBody    = B.unpack content
          }
    simpleHTTP_ con request
  rsp <- hoistE $ left ConnectionException rawRsp
  assert200Response path rsp
  return rsp

assert200Response :: URI -> Response_String -> IO ()
assert200Response path rsp =
    when (rspCode rsp /= (2,0,0)) $
        let httpExc = HTTPResponseException $ "mmRequest: expected 200 response, got " <>
                                              (show $ rspCode rsp)
        in case eitherDecode $ BL.pack $ rspBody rsp of
            Right (Object o) ->
                case HM.lookup "message" o of
                    Just (String msg) ->
                        let newMsg = (T.pack $ "Error requesting " <> show path <> ": ") <> msg
                        in throwIO $ MattermostServerError newMsg
                    _ -> throwIO $ httpExc
            _ -> throwIO $ httpExc
