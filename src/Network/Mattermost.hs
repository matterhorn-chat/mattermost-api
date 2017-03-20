{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Mattermost
( -- * Types
  -- ** Mattermost-Related Types
  Login(..)
, Token
, Hostname
, Port
, ConnectionData
, Id(..)
, User(..)
, UserId(..)
, InitialLoad(..)
, Team(..)
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
, mmUsersCreateWithToken
, mmPost
, mmUpdatePost
, mmExecute
, mmGetConfig -- Requires Admin access
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
import qualified Data.Sequence as Seq
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
import qualified Data.Text as T
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
mmGetJSONBody :: FromJSON t => String -> Response_String -> IO (Value, t)
mmGetJSONBody label rsp = do
  contentType <- mmGetHeader rsp HdrContentType
  assertE (contentType ~= "application/json")
          (ContentTypeException
            ("mmGetJSONBody: " ++ label ++ ": " ++
             "Expected content type 'application/json'" ++
             " found " ++ contentType))

  -- XXX: Good for seeing the json wireformat that mattermost uses
  -- putStrLn (rspBody rsp)
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
mmLogin :: ConnectionData -> Login -> IO (Either LoginFailureException (Token, User))
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
      return (Right (Token token, value))

-- | Fire off a login attempt. Note: We get back more than just the auth token.
-- We also get all the server-side configuration data for the user.
mmGetInitialLoad :: ConnectionData -> Token -> IO InitialLoad
mmGetInitialLoad cd token =
  mmDoRequest cd "mmGetInitialLoad" token "/api/v3/users/initial_load"

-- | Requires an authenticated user. Returns the full list of teams.
mmGetTeams :: ConnectionData -> Token -> IO (HashMap TeamId Team)
mmGetTeams cd token =
  mmDoRequest cd "mmGetTeams" token "/api/v3/teams/all"

mmCreateTeam :: ConnectionData -> Token -> TeamsCreate -> IO Team
mmCreateTeam cd token payload = do
  let path = "/api/v3/teams/create"
  uri <- mmPath path
  runLogger cd "mmCreateTeam" $
    HttpRequest POST path (Just (toJSON payload))
  rsp <- mmPOST cd token uri payload
  (val, r) <- mmGetJSONBody "Team" rsp
  runLogger cd "mmCreateTeam" $
    HttpResponse 200 path (Just val)
  return r

-- | Requires an authenticated user. Returns the full list of channels
-- for a given team of which the user is a member
mmGetChannels :: ConnectionData -> Token -> TeamId -> IO Channels
mmGetChannels cd token teamid = mmDoRequest cd "mmGetChannels" token $
  printf "/api/v3/teams/%s/channels/" (idString teamid)

-- | Requires an authenticated user. Returns the channels for a team of
-- which the user is not already a member
mmGetMoreChannels :: ConnectionData -> Token -> TeamId -> IO Channels
mmGetMoreChannels cd token teamid = mmDoRequest cd "mmGetMoreChannels" token $
  printf "/api/v3/teams/%s/channels/more" (idString teamid)

-- | Requires an authenticated user. Returns the details of a
-- specific channel.
mmGetChannel :: ConnectionData -> Token
             -> TeamId
             -> ChannelId
             -> IO ChannelWithData
mmGetChannel cd token teamid chanid = mmWithRequest cd "mmGetChannel" token
  (printf "/api/v3/teams/%s/channels/%s/"
          (idString teamid)
          (idString chanid))
  return

mmUpdateLastViewedAt :: ConnectionData -> Token
                     -> TeamId
                     -> ChannelId
                     -> IO ()
mmUpdateLastViewedAt cd token teamid chanid = do
  let uri = printf "/api/v3/teams/%s/channels/%s/update_last_viewed_at"
                   (idString teamid)
                   (idString chanid)
  path <- mmPath uri
  runLogger cd "mmUpdateLastViewedAt" $
    HttpRequest POST uri Nothing
  _ <- mmRawPOST cd token path ""
  runLogger cd "mmUpdateLastViewedAt" $
    HttpResponse 200 uri Nothing
  return ()

mmJoinChannel :: ConnectionData -> Token
              -> TeamId
              -> ChannelId
              -> IO ()
mmJoinChannel cd token teamid chanid = do
  let path = printf "/api/v3/teams/%s/channels/%s/join"
                   (idString teamid)
                   (idString chanid)
  uri <- mmPath path
  runLogger cd "mmJoinChannel" $
    HttpRequest POST path Nothing
  rsp <- mmPOST cd token uri (""::T.Text)
  (val, (_::Channel)) <- mmGetJSONBody "Channel" rsp
  runLogger cd "mmJoinChannel" $
    HttpResponse 200 path (Just val)
  return ()

mmLeaveChannel :: ConnectionData -> Token
               -> TeamId
               -> ChannelId
               -> IO ()
mmLeaveChannel cd token teamid chanid = do
  let path = printf "/api/v3/teams/%s/channels/%s/leave"
                   (idString teamid)
                   (idString chanid)
      payload = HM.fromList [("id" :: T.Text, chanid)]
  uri <- mmPath path
  runLogger cd "mmLeaveChannel" $
    HttpRequest POST path (Just (toJSON payload))
  rsp <- mmPOST cd token uri payload
  (val, (_::HM.HashMap T.Text ChannelId)) <- mmGetJSONBody "Channel name/ID map" rsp
  runLogger cd "mmCreateDirect" $
    HttpResponse 200 path (Just val)
  return ()

mmGetPosts :: ConnectionData -> Token
           -> TeamId
           -> ChannelId
           -> Int -- offset in the backlog, 0 is most recent
           -> Int -- try to fetch this many
           -> IO Posts
mmGetPosts cd token teamid chanid offset limit =
  mmDoRequest cd "mmGetPosts" token $
  printf "/api/v3/teams/%s/channels/%s/posts/page/%d/%d"
         (idString teamid)
         (idString chanid)
         offset
         limit

mmGetPostsSince :: ConnectionData
           -> Token
           -> TeamId
           -> ChannelId
           -> UTCTime
           -> IO Posts
mmGetPostsSince cd token teamid chanid since =
  mmDoRequest cd "mmGetPostsSince" token $
  printf "/api/v3/teams/%s/channels/%s/posts/since/%d"
         (idString teamid)
         (idString chanid)
         (utcTimeToMilliseconds since :: Int)

mmGetPost :: ConnectionData -> Token
          -> TeamId
          -> ChannelId
          -> PostId
          -> IO Posts
mmGetPost cd token teamid chanid postid = do
  let path = printf "/api/v3/teams/%s/channels/%s/posts/%s/get"
             (idString teamid)
             (idString chanid)
             (idString postid)
  uri <- mmPath path
  rsp <- mmRequest cd token uri
  (raw, json) <- mmGetJSONBody "Posts" rsp
  runLogger cd "mmGetPost" $
    HttpResponse 200 path (Just raw)
  return json

mmGetPostsAfter :: ConnectionData -> Token
                -> TeamId
                -> ChannelId
                -> PostId
                -> Int -- offset in the backlog, 0 is most recent
                -> Int -- try to fetch this many
                -> IO Posts
mmGetPostsAfter cd token teamid chanid postid offset limit =
  mmDoRequest cd "mmGetPosts" token $
  printf "/api/v3/teams/%s/channels/%s/posts/%s/after/%d/%d"
         (idString teamid)
         (idString chanid)
         (idString postid)
         offset
         limit

mmGetPostsBefore :: ConnectionData -> Token
                -> TeamId
                -> ChannelId
                -> PostId
                -> Int -- offset in the backlog, 0 is most recent
                -> Int -- try to fetch this many
                -> IO Posts
mmGetPostsBefore cd token teamid chanid postid offset limit =
  mmDoRequest cd "mmGetPosts" token $
  printf "/api/v3/teams/%s/channels/%s/posts/%s/before/%d/%d"
         (idString teamid)
         (idString chanid)
         (idString postid)
         offset
         limit

mmGetFileInfo :: ConnectionData -> Token
              -> FileId
              -> IO FileInfo
mmGetFileInfo cd token fileId =
  mmDoRequest cd "mmGetFileInfo" token $
  printf "/api/v3/files/%s/get_info" (idString fileId)

mmGetUser :: ConnectionData -> Token -> UserId -> IO User
mmGetUser cd token userid = mmDoRequest cd "mmGetUser" token $
  printf "/api/v3/users/%s/get" (idString userid)

mmGetUsers :: ConnectionData -> Token -> Int -> Int -> IO (HashMap UserId User)
mmGetUsers cd token offset limit =
  mmDoRequest cd "mmGetUsers" token $
    printf "/api/v3/users/%d/%d" offset limit

mmGetTeamMembers :: ConnectionData -> Token -> TeamId -> IO Value
mmGetTeamMembers cd token teamid = mmDoRequest cd "mmGetTeamMembers" token $
  printf "/api/v3/teams/members/%s" (idString teamid)

mmGetChannelMembers :: ConnectionData -> Token -> TeamId -> IO (Seq.Seq ChannelData)
mmGetChannelMembers cd token teamid = mmDoRequest cd "mmGetChannelMembers" token $
  printf "/api/v3/teams/%s/channels/members" (idString teamid)

mmGetProfilesForDMList :: ConnectionData -> Token -> TeamId
                       -> IO (HashMap UserId User)
mmGetProfilesForDMList cd token teamid =
  mmDoRequest cd "mmGetProfilesForDMList" token $
    printf "/api/v3/users/profiles_for_dm_list/%s" (idString teamid)

mmGetMe :: ConnectionData -> Token -> IO Value
mmGetMe cd token = mmDoRequest cd "mmGetMe" token "/api/v3/users/me"

mmGetProfiles :: ConnectionData -> Token
              -> TeamId -> IO (HashMap UserId User)
mmGetProfiles cd token teamid = mmDoRequest cd "mmGetProfiles" token $
  printf "/api/v3/teams/%s/users/%d/%d" (idString teamid) (0::Int) (10000::Int)

mmGetStatuses :: ConnectionData -> Token -> IO (HashMap UserId T.Text)
mmGetStatuses cd token = mmDoRequest cd "mmGetStatuses" token $
  printf "/api/v3/users/status"

-- POST /api/v3/teams/{}/create_direct with {"user_id": _}
mmCreateDirect :: ConnectionData -> Token -> TeamId -> UserId -> IO Channel
mmCreateDirect cd token teamid userid = do
  let path = printf "/api/v3/teams/%s/channels/create_direct" (idString teamid)
      payload = HM.fromList [("user_id" :: T.Text, userid)]
  uri <- mmPath path
  runLogger cd "mmCreateDirect" $
    HttpRequest POST path (Just (toJSON payload))
  rsp <- mmPOST cd token uri payload
  (val, r) <- mmGetJSONBody "Channel" rsp
  runLogger cd "mmCreateDirect" $
    HttpResponse 200 path (Just val)
  return r

-- { name, display_name, purpose, header }
mmCreateChannel :: ConnectionData -> Token -> TeamId -> MinChannel -> IO Channel
mmCreateChannel cd token teamid payload = do
  let path = printf "/api/v3/teams/%s/channels/create" (idString teamid)
  uri <- mmPath path
  runLogger cd "mmCreateChannel" $
    HttpRequest POST path (Just (toJSON payload))
  rsp <- mmPOST cd token uri payload
  (val, r) <- mmGetJSONBody "Channel" rsp
  runLogger cd "mmCreateChannel" $
    HttpResponse 200 path (Just val)
  return r

mmDeleteChannel :: ConnectionData -> Token -> TeamId -> ChannelId -> IO ()
mmDeleteChannel cd token teamid chanid = do
  let path = printf "/api/v3/teams/%s/channels/%s/delete"
               (idString teamid) (idString chanid)
  uri <- mmPath path
  runLogger cd "mmDeleteChannel" $
    HttpRequest POST path Nothing
  _ <- mmRawPOST cd token uri ""
  runLogger cd "mmDeleteChannel" $
    HttpResponse 200 path Nothing
  return ()

mmDeletePost :: ConnectionData
             -> Token
             -> TeamId
             -> ChannelId
             -> PostId
             -> IO ()
mmDeletePost cd token teamid chanid postid = do
  let path   = printf "/api/v3/teams/%s/channels/%s/posts/%s/delete"
                      (idString teamid)
                      (idString chanid)
                      (idString postid)
  uri <- mmPath path
  runLogger cd "mmDeletePost" $
    HttpRequest POST path Nothing
  rsp <- mmPOST cd token uri ([]::[String])
  (_, _::Value) <- mmGetJSONBody "Post" rsp
  runLogger cd "mmDeletePost" $
    HttpResponse 200 path Nothing
  return ()

mmUpdatePost :: ConnectionData
             -> Token
             -> TeamId
             -> Post
             -> IO Post -- TODO: return something informative for failures
mmUpdatePost cd token teamid post = do
  let chanid = postChannelId post
      path   = printf "/api/v3/teams/%s/channels/%s/posts/update"
                      (idString teamid)
                      (idString chanid)
  uri <- mmPath path
  runLogger cd "mmUpdatePost" $
    HttpRequest POST path (Just (toJSON post))
  rsp <- mmPOST cd token uri post
  (val, r) <- mmGetJSONBody "Post" rsp
  runLogger cd "mmUpdatePost" $
    HttpResponse 200 path (Just (val))
  return r

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
  runLogger cd "mmPost" $
    HttpRequest POST path (Just (toJSON post))
  rsp <- mmPOST cd token uri post
  (val, r) <- mmGetJSONBody "Post" rsp
  runLogger cd "mmPost" $
    HttpResponse 200 path (Just (val))
  return r

mmGetConfig :: ConnectionData
            -> Token
            -> IO Value
mmGetConfig cd token =
  mmDoRequest cd "mmGetConfig" token "/api/v3/admin/config"

mmSaveConfig :: ConnectionData
             -> Token
             -> Value
             -> IO ()
mmSaveConfig cd token config = do
  let path = "/api/v3/admin/save_config"
  uri <- mmPath path
  runLogger cd "mmSaveConfig" $
    HttpRequest POST path (Just config)
  _ <- mmPOST cd token uri config
  runLogger cd "mmSaveConfig" $
    HttpResponse 200 path Nothing
  return ()

mmChannelAddUser :: ConnectionData
                 -> Token
                 -> TeamId
                 -> ChannelId
                 -> UserId
                 -> IO ChannelData
mmChannelAddUser cd token teamid chanId uId = do
  let path = printf "/api/v3/teams/%s/channels/%s/add"
                    (idString teamid)
                    (idString chanId)
      req = object ["user_id" .= uId]
  uri <- mmPath path
  runLogger cd "mmChannelAddUser" $
    HttpRequest POST path (Just req)
  rsp <- mmPOST cd token uri req
  (val, r) <- mmGetJSONBody "ChannelData" rsp
  runLogger cd "mmChannelAddUser" $
    HttpResponse 200 path (Just val)
  return r

mmTeamAddUser :: ConnectionData
              -> Token
              -> TeamId
              -> UserId
              -> IO ()
mmTeamAddUser cd token teamid uId = do
  let path = printf "/api/v3/teams/%s/add_user_to_team"
                    (idString teamid)
      req  = object ["user_id" .= uId]
  uri <- mmPath path
  runLogger cd "mmTeamAddUser" $
    HttpRequest POST path (Just req)
  _ <- mmPOST cd token uri req
  runLogger cd "mmTeamAddUSer" $
    HttpResponse 200 path Nothing
  return ()

mmExecute :: ConnectionData
          -> Token
          -> TeamId
          -> MinCommand
          -> IO Value -- XXX: what to return here?
mmExecute cd token teamid command = do
  let path   = printf "/api/v3/teams/%s/commands/execute"
                      (idString teamid)
  uri <- mmPath path
  runLogger cd "mmExecute" $
    HttpRequest POST path (Just (toJSON command))
  rsp <- mmPOST cd token uri command
  (val, r) <- mmGetJSONBody "Value" rsp
  runLogger cd "mmExecute" $
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

mmUsersCreateWithToken :: ConnectionData
                       -> Token
                       -> UsersCreate
                       -> IO User
mmUsersCreateWithToken cd token usersCreate = do
  let path = "/api/v3/users/create"
  uri <- mmPath path
  runLogger cd "mmUsersCreateWithToken" $
    HttpRequest POST path (Just (toJSON usersCreate))
  rsp <- mmPOST cd token uri usersCreate
  (val, r) <- mmGetJSONBody "User" rsp
  runLogger cd "mmUsersCreateWithToken" $
    HttpResponse 200 path (Just (val))
  return r

mmGetReactionsForPost :: ConnectionData
                      -> Token
                      -> TeamId
                      -> ChannelId
                      -> PostId
                      -> IO [Reaction]
mmGetReactionsForPost cd token tId cId pId = do
  let path = printf "/api/v3/teams/%s/channels/%s/posts/%s/reactions"
                    (idString tId)
                    (idString cId)
                    (idString pId)
  mmDoRequest cd "mmGetReactionsForPost" token path

-- | This is for making a generic authenticated request.
mmRequest :: ConnectionData -> Token -> URI -> IO Response_String
mmRequest cd token path = do
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
            => ConnectionData
            -> String
            -> Token
            -> String
            -> IO t
mmDoRequest cd fnname token path = mmWithRequest cd fnname token path return

-- The slightly more general variant
mmWithRequest :: FromJSON t
              => ConnectionData
              -> String
              -> Token
              -> String
              -> (t -> IO a)
              -> IO a
mmWithRequest cd fnname token path action = do
  uri  <- mmPath path
  runLogger cd fnname $
    HttpRequest GET path Nothing
  rsp  <- mmRequest cd token uri
  (raw,json) <- mmGetJSONBody fnname rsp
  runLogger cd fnname $
    HttpResponse 200 path (Just raw)
  action json

mmPOST :: ToJSON t => ConnectionData -> Token -> URI -> t -> IO Response_String
mmPOST cd token path json =
  mmRawPOST cd token path (BL.toStrict (encode json))

mmSetChannelHeader :: ConnectionData -> Token -> TeamId -> ChannelId -> T.Text -> IO Channel
mmSetChannelHeader cd token teamid chanid header = do
  let path = printf "/api/v3/teams/%s/channels/update_header"
                    (idString teamid)
  uri <- mmPath path
  let req = SetChannelHeader chanid header
  runLogger cd "mmSetChannelHeader" $
    HttpRequest POST path (Just (toJSON req))
  rsp <- mmPOST cd token uri req
  (_, r) <- mmGetJSONBody "Channel" rsp
  return r

mmRawPOST :: ConnectionData -> Token -> URI -> B.ByteString -> IO Response_String
mmRawPOST cd token path content = do
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
