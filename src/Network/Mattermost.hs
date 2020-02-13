{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
module Network.Mattermost
( -- * Types
  -- ** Mattermost-Related Types (deprecated: use Network.Mattermost.Types instead)
  -- n.b. the deprecation notice is in that haddock header because we're
  -- still waiting for https://ghc.haskell.org/trac/ghc/ticket/4879 ...
  ConnectionPoolConfig(..)
, Login(..)
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
, ConnectionType(..)
, MinChannel(..)
, UsersCreate(..)
, Post(..)
, PostType(..)
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
, Preference(..)
, PreferenceCategory(..)
, PreferenceName(..)
, PreferenceValue(..)
, FlaggedPost(..)
, preferenceToFlaggedPost
-- ** Log-related types
, Logger
, LogEvent(..)
, LogEventType(..)
, withLogger
, noLogger
-- * Typeclasses
, HasId(..)
-- * HTTP API Functions
, defaultConnectionPoolConfig
, mkConnectionData
, initConnectionData
, mmCloseSession
, mmLogin
, mmCreateDirect
, mmCreateChannel
, mmCreateGroupChannel
, mmCreateTeam
, mmDeleteChannel
, mmLeaveChannel
, mmJoinChannel
, mmGetTeams
, mmGetChannels
, mmGetAllChannelsForUser
, mmGetAllChannelDataForUser
, mmGetAllChannelsWithDataForUser
, mmGetMoreChannels
, mmGetChannel
, mmViewChannel
, mmDeletePost
, mmGetPost
, mmGetPosts
, mmGetPostsSince
, mmGetPostsBefore
, mmGetPostsAfter
, mmSearchPosts
, mmGetReactionsForPost
, mmGetFileInfo
, mmGetFile
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
, mmChannelRemoveUser
, mmTeamAddUser
, mmUsersCreate
, mmUsersCreateWithSession
, mmPost
, mmUpdatePost
, mmExecute
, mmGetConfig
, mmGetClientConfig
, mmSetPreferences
, mmSavePreferences
, mmDeletePreferences
, mmFlagPost
, mmUnflagPost
, mmGetFlaggedPosts
, mmGetMyPreferences
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
import           Network.HTTP.Headers ( HeaderName(..)
                                      , mkHeader
                                      )
import           Network.HTTP.Base ( Request(..)
                                   , RequestMethod(..)
                                   , defaultUserAgent
                                   , Response_String
                                   , Response(..) )
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
import           Data.Maybe ( maybeToList, fromJust )
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Control.Arrow ( left )

import           Network.Mattermost.Connection (assertJSONResponse, mmGetHeader)
import           Network.Mattermost.Exceptions
import           Network.Mattermost.Util
import           Network.Mattermost.Types.Base
import           Network.Mattermost.Types.Internal
import           Network.Mattermost.Types


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
  assertJSONResponse rsp

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
--
-- route: @\/api\/v3\/users\/login@
mmLogin :: ConnectionData -> Login -> IO (Either LoginFailureException (Session, User))
mmLogin cd login = do
  let rawPath = "/api/v3/users/login"
  path <- mmPath rawPath
  runLogger cd "mmLogin" $
    HttpRequest GET rawPath (Just (toJSON $ login { password = "<elided>" }))
  rsp  <- mmUnauthenticatedHTTPPost cd path login
  if (rspCode rsp /= (2,0,0))
    then do
        let eMsg = "Server returned unexpected " <> showRespCode (rspCode rsp) <> " response"
        return $ Left $ LoginFailureException eMsg
    else do
      token <- mmGetHeader   rsp (HdrCustom "Token")
      (raw, value) <- mmGetJSONBody "User" rsp
      runLogger cd "mmLogin" $
        HttpResponse 200 rawPath (Just raw)
      return (Right (Session cd (Token token), value))

showRespCode :: (Int, Int, Int) -> String
showRespCode (a, b, c) = concat $ show <$> [a, b, c]

-- | Fire off a login attempt. Note: We get back more than just the auth token.
-- We also get all the server-side configuration data for the user.
--
-- route: @\/api\/v3\/users\/initial_load@
mmGetInitialLoad :: Session -> IO InitialLoad
mmGetInitialLoad sess =
  mmDoRequest sess "mmGetInitialLoad" "/api/v3/users/initial_load"

-- | Requires an authenticated user. Returns the full list of teams.
--
-- route: @\/api\/v3\/teams\/all@
mmGetTeams :: Session -> IO (HashMap TeamId Team)
mmGetTeams sess =
  mmDoRequest sess "mmGetTeams" "/api/v3/teams/all"

-- |
-- route: @\/api\/v3\/teams\/create@
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
--
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/@
mmGetChannels :: Session -> TeamId -> IO Channels
mmGetChannels sess teamid = mmDoRequest sess "mmGetChannels" $
  printf "/api/v3/teams/%s/channels/" (idString teamid)

-- | Requires an authenticated user. Returns the channels for a team of
-- which the user is not already a member
--
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/more\/{offset}\/{limit}@
mmGetMoreChannels :: Session -> TeamId -> Int -> Int -> IO Channels
mmGetMoreChannels sess teamid offset limit =
  mmDoRequest sess "mmGetMoreChannels" $
    printf "/api/v3/teams/%s/channels/more/%d/%d"
           (idString teamid)
           offset
           limit

-- | Requires an authenticated user. Returns the details of a
-- specific channel.
--
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}@
mmGetChannel :: Session
             -> TeamId
             -> ChannelId
             -> IO ChannelWithData
mmGetChannel sess teamid chanid = mmWithRequest sess "mmGetChannel"
  (printf "/api/v3/teams/%s/channels/%s/"
          (idString teamid)
          (idString chanid))
  return

-- | Get channel/user metadata in bulk.
mmGetAllChannelDataForUser :: Session
                           -> TeamId
                           -> UserId
                           -> IO (Seq.Seq ChannelData)
mmGetAllChannelDataForUser sess teamid userid =
    mmDoRequest sess "mmGetAllChannelDataForUser" $
      printf "/api/v4/users/%s/teams/%s/channels/members"
             (idString userid)
             (idString teamid)

mmGetAllChannelsForUser :: Session
                        -> TeamId
                        -> UserId
                        -> IO (Seq.Seq Channel)
mmGetAllChannelsForUser sess teamid userid =
    mmDoRequest sess "mmGetAllChannelsForUser" $
      printf "/api/v4/users/%s/teams/%s/channels"
             (idString userid)
             (idString teamid)

mmGetAllChannelsWithDataForUser :: Session
                                -> TeamId
                                -> UserId
                                -> IO (HM.HashMap ChannelId ChannelWithData)
mmGetAllChannelsWithDataForUser sess teamid userid = do
    chans <- mmGetAllChannelsForUser sess teamid userid
    datas <- mmGetAllChannelDataForUser sess teamid userid

    let dataMap = HM.fromList $ F.toList $ (\d -> (channelDataChannelId d, d)) <$> datas
        mkPair chan = (getId chan, ChannelWithData chan $ fromJust $ HM.lookup (getId chan) dataMap)

    return $ HM.fromList $ F.toList $ mkPair <$> chans

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/view@
mmViewChannel :: Session
              -> TeamId
              -> ChannelId       -- ^ channel to view
              -> Maybe ChannelId -- ^ previous channel
              -> IO ()
mmViewChannel sess teamid chanid previd = do
  let path    = printf "/api/v3/teams/%s/channels/view"
                       (idString teamid)
      prev    = maybeToList (("prev_channel_id" :: T.Text,) <$> previd)
      payload = HM.fromList $ [("channel_id" :: T.Text, chanid)] ++ prev
  uri <- mmPath path
  runLoggerS sess "mmViewChannel" $
    HttpRequest POST path (Just (toJSON payload))
  _ <- mmPOST sess uri payload
  runLoggerS sess "mmViewChannel" $
    HttpResponse 200 path Nothing
  return ()

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}\/join@
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

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}\/leave@
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

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}\/posts\/page\/{offset}\/{limit}@
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

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}\/posts\/since\/{utc_time}@
mmGetPostsSince :: Session
           -> TeamId
           -> ChannelId
           -> ServerTime
           -> IO Posts
mmGetPostsSince sess teamid chanid since =
  mmDoRequest sess "mmGetPostsSince" $
  printf "/api/v3/teams/%s/channels/%s/posts/since/%d"
         (idString teamid)
         (idString chanid)
         (timeToServer since)

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}\/posts\/{post_id}\/get@
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

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}\/posts\/{post_id}\/after\/{offset}\/{limit}@
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

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}\/posts\/{post_id}\/before\/{offset}\/{limit}@
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

-- |
-- route: @\/api\/v4\/teams\/{team_id}\/posts\/search@
mmSearchPosts :: Session
              -> TeamId
              -> T.Text
              -> Bool
              -> IO Posts
mmSearchPosts sess teamid terms isOrSearch = do
  let path = printf "/api/v4/teams/%s/posts/search" $ idString teamid
  uri <- mmPath path
  let req = SearchPosts terms isOrSearch
  runLoggerS sess "mmSearchPosts" $
    HttpRequest POST path (Just (toJSON req))
  rsp <- mmPOST sess uri req
  (raw, value) <- mmGetJSONBody "SearchPostsResult" rsp
  runLoggerS sess "mmSearchPosts" $
    HttpResponse 200 path (Just raw)
  return value

-- |
-- route: @\/api\/v3\/files\/{file_id}\/get_info@
mmGetFileInfo :: Session
              -> FileId
              -> IO FileInfo
mmGetFileInfo sess fileId =
  mmDoRequest sess "mmGetFileInfo" $
  printf "/api/v3/files/%s/get_info" (idString fileId)

-- |
-- route: @\/api\/v4\/files\/{file_id}@
mmGetFile :: Session
          -> FileId
          -> IO B.ByteString
mmGetFile sess@(Session cd _) fileId = do
  let path = printf "/api/v4/files/%s" (idString fileId)
  uri  <- mmPath path
  runLogger cd "mmGetFile" $
    HttpRequest GET path Nothing
  rsp  <- mmRequest sess uri
  return (B.pack (rspBody rsp))

-- |
-- route: @\/api\/v3\/users\/{user_id}\/get@
mmGetUser :: Session -> UserId -> IO User
mmGetUser sess userid = mmDoRequest sess "mmGetUser" $
  printf "/api/v3/users/%s/get" (idString userid)

-- |
-- route: @\/api\/v3\/users\/{offset}\/{limit}@
mmGetUsers :: Session -> Int -> Int -> IO (HashMap UserId User)
mmGetUsers sess offset limit =
  mmDoRequest sess "mmGetUsers" $
    printf "/api/v3/users/%d/%d" offset limit

-- |
-- route: @\/api\/v3\/teams\/members\/{team_id}@
mmGetTeamMembers :: Session -> TeamId -> IO (Seq.Seq TeamMember)
mmGetTeamMembers sess teamid = mmDoRequest sess "mmGetTeamMembers" $
  printf "/api/v3/teams/members/%s" (idString teamid)

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}\/users\/{offset}\/{limit}@
mmGetChannelMembers :: Session
                    -> TeamId
                    -> ChannelId
                    -> Int
                    -> Int
                    -> IO (HashMap UserId User)
mmGetChannelMembers sess teamid chanid offset limit = mmDoRequest sess "mmGetChannelMembers" $
  printf "/api/v3/teams/%s/channels/%s/users/%d/%d"
         (idString teamid)
         (idString chanid)
         offset
         limit

-- |
-- route: @\/api\/v3\/users\/profiles_for_dm_list\/{team_id}@
mmGetProfilesForDMList :: Session -> TeamId
                       -> IO (HashMap UserId User)
mmGetProfilesForDMList sess teamid =
  mmDoRequest sess "mmGetProfilesForDMList" $
    printf "/api/v3/users/profiles_for_dm_list/%s" (idString teamid)

-- |
-- route: @\/api\/v3\/users\/me@
mmGetMe :: Session -> IO User
mmGetMe sess = mmDoRequest sess "mmGetMe" "/api/v3/users/me"

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/users\/{offset}\/{limit}@
mmGetProfiles :: Session
              -> TeamId
              -> Int
              -> Int
              -> IO (HashMap UserId User)
mmGetProfiles sess teamid offset limit = mmDoRequest sess "mmGetProfiles" $
  printf "/api/v3/teams/%s/users/%d/%d"
         (idString teamid)
         offset
         limit

-- |
-- route: @\/api\/v3\/users\/status@
mmGetStatuses :: Session -> IO (HashMap UserId T.Text)
mmGetStatuses sess = mmDoRequest sess "mmGetStatuses" $
  printf "/api/v3/users/status"

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/create_direct@
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
-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/create@
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

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}\/delete@
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

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}\/posts\/{post_id}\/delete@
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

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}\/posts\/update@
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

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}\/posts\/create@
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
--
-- route: @\/api\/v3\/admin\/config@
mmGetConfig :: Session
            -> IO Value
mmGetConfig sess =
  mmDoRequest sess "mmGetConfig" "/api/v3/admin/config"

-- | Get a subset of the server configuration needed by the client. Does not
-- require administrative permission. The format query parameter is currently
-- required with the value of "old".
--
-- route: @\/api\/v4\/config\/client@
mmGetClientConfig :: Session
                  -> IO Value
mmGetClientConfig sess =
  mmDoRequest sess "mmGetClientConfig" "/api/v4/config/client?format=old"

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

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}\/add@
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

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/add_user_to_team@
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

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/commands\/execute@
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

-- |
-- route: @\/api\/v3\/users\/create@
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

-- |
-- route: @\/api\/v3\/users\/create@
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

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}\/posts\/{post_id}\/reactions@
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

mmSetPreferences :: Session
                 -> UserId
                 -> Seq.Seq Preference
                 -> IO ()
mmSetPreferences sess uId prefs = do
  uri <- mmPath $ printf "/api/v4/users/%s/preferences" (idString uId)
  _ <- mmPUT sess uri prefs
  return ()

-- |
-- route: @\/api\/v3\/preferences\/save@
mmSavePreferences :: Session
                  -> Seq.Seq Preference
                  -> IO ()
mmSavePreferences sess pref = do
  uri <- mmPath "/api/v3/preferences/save"
  _ <- mmPOST sess uri pref
  return ()

-- |
-- route: @\/api\/v3\/preferences\/save@
mmDeletePreferences :: Session
                  -> Seq.Seq Preference
                  -> IO ()
mmDeletePreferences sess pref = do
  uri <- mmPath "/api/v3/preferences/delete"
  _ <- mmPOST sess uri pref
  return ()

-- |
-- route: @\/api\/v3\/preferences\/save@
--
-- This is a convenience function for a particular use of
-- 'mmSavePreference'
mmFlagPost :: Session
           -> UserId
           -> PostId
           -> IO ()
mmFlagPost sess uId pId = do
  let flaggedPost =
        FlaggedPost
          { flaggedPostUserId = uId
          , flaggedPostId     = pId
          , flaggedPostStatus = True
          }
  let rawPath = "/api/v3/preferences/save"
  runLoggerS sess "mmFlagPost" $
    HttpRequest POST rawPath (Just (toJSON [flaggedPost]))
  uri <- mmPath rawPath
  _ <- mmPOST sess uri (Seq.singleton flaggedPost)
  return ()

-- |
-- route: @\/api\/v3\/preferences\/save@
--
-- This is a convenience function for a particular use of
-- 'mmSavePreference'
mmUnflagPost :: Session
             -> UserId
             -> PostId
             -> IO ()
mmUnflagPost sess uId pId = do
  let flaggedPost =
        FlaggedPost
          { flaggedPostUserId = uId
          , flaggedPostId     = pId
          , flaggedPostStatus = True
          }
  let rawPath = "/api/v3/preferences/delete"
  runLoggerS sess "mmUnflagPost" $
    HttpRequest POST rawPath (Just (toJSON [flaggedPost]))
  uri <- mmPath rawPath
  _ <- mmPOST sess uri (Seq.singleton flaggedPost)
  return ()

mmGetFlaggedPosts :: Session
                  -> UserId
                  -> IO Posts
mmGetFlaggedPosts sess uId =
  let path = printf "/api/v4/users/%s/posts/flagged" (idString uId)
  in mmDoRequest sess "mmGetFlaggedPosts" path

mmGetMyPreferences :: Session
                -> IO (Seq.Seq Preference)
mmGetMyPreferences sess =
  mmDoRequest sess "mmMyPreferences" "/api/v4/users/me/preferences"

-- | Remove the specified user from the specified channel.
mmChannelRemoveUser :: Session
                    -> ChannelId
                    -> UserId
                    -> IO ()
mmChannelRemoveUser sess cId uId =
  let path = printf "/api/v4/channels/%s/members/%s" (idString cId) (idString uId)
  in mmDeleteRequest sess =<< mmPath path

-- | Create a group channel containing the specified users in addition
-- to the user making the request.
mmCreateGroupChannel :: Session
                     -> [UserId]
                     -> IO Channel
mmCreateGroupChannel sess@(Session cd _) uIds = do
  let path = "/api/v4/channels/group"
      fnname = "mmCreateGroupChannel"
  uri <- mmPath path
  runLoggerS sess fnname $
    HttpRequest POST path (Just (toJSON uIds))
  rsp <- mmPOST sess uri uIds
  (raw, json) <- mmGetJSONBody fnname rsp
  runLogger cd fnname $
    HttpResponse 200 path (Just raw)
  return json

mmDeleteRequest :: Session -> URI -> IO ()
mmDeleteRequest (Session cd token) path = do
  rawRsp <- withConnection cd $ \con -> do
    let request = Request
          { rqURI     = path
          , rqMethod  = DELETE
          , rqHeaders = [ mkHeader HdrAuthorization ("Bearer " ++ getTokenString token)
                        , mkHeader HdrHost          (T.unpack $ cdHostname cd)
                        , mkHeader HdrUserAgent     defaultUserAgent
                        ] ++ autoCloseToHeader (cdAutoClose cd)
          , rqBody    = ""
          }
    simpleHTTP_ con request
  rsp <- hoistE $ left ConnectionException rawRsp
  assert200Response path rsp

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

mmPUT :: ToJSON t => Session -> URI -> t -> IO Response_String
mmPUT sess path json =
  mmRawPUT sess path (BL.toStrict (encode json))

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/update_header@
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

mmRawPUT :: Session -> URI -> B.ByteString -> IO Response_String
mmRawPUT (Session cd token) path content = do
  rawRsp <- withConnection cd $ \con -> do
    let contentLength = B.length content
        request       = Request
          { rqURI     = path
          , rqMethod  = PUT
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
    let is20x (2, 0, _) = True
        is20x _ = False
    in when (not $ is20x $ rspCode rsp) $
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

mmCloseSession :: Session -> IO ()
mmCloseSession (Session cd _) = destroyConnectionData cd
