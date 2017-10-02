{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Mattermost
( -- * Types
  -- ** Mattermost-Related Types (deprecated: use Network.Mattermost.Types instead)
  -- n.b. the deprecation notice is in that haddock header because we're
  -- still waiting for https://ghc.haskell.org/trac/ghc/ticket/4879 ...
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
, mmTeamAddUser
, mmUsersCreate
, mmUsersCreateWithSession
, mmPost
, mmUpdatePost
, mmExecute
, mmGetConfig
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
import           Data.Maybe ( maybeToList, fromJust )
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Control.Arrow ( left )

import           Network.Mattermost.Exceptions
import           Network.Mattermost.Util
import           Network.Mattermost.Types.Base
import           Network.Mattermost.Types.Internal
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
mmCreateTeam :: TeamsCreate -> Session -> IO Team
mmCreateTeam payload sess = do
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
mmGetChannels :: TeamId -> Session -> IO Channels
mmGetChannels teamid sess = mmDoRequest sess "mmGetChannels" $
  printf "/api/v3/teams/%s/channels/" (idString teamid)

-- | Requires an authenticated user. Returns the channels for a team of
-- which the user is not already a member
--
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/more\/{offset}\/{limit}@
mmGetMoreChannels :: TeamId -> Int -> Int -> Session -> IO Channels
mmGetMoreChannels teamid offset limit sess =
  mmDoRequest sess "mmGetMoreChannels" $
    printf "/api/v3/teams/%s/channels/more/%d/%d"
           (idString teamid)
           offset
           limit

-- | Requires an authenticated user. Returns the details of a
-- specific channel.
--
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}@
mmGetChannel :: TeamId
             -> ChannelId
             -> Session
             -> IO ChannelWithData
mmGetChannel teamid chanid sess = mmWithRequest sess "mmGetChannel"
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
mmViewChannel :: TeamId
              -> ChannelId       -- ^ channel to view
              -> Maybe ChannelId -- ^ previous channel
              -> Session
              -> IO ()
mmViewChannel teamid chanid previd sess = do
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
mmJoinChannel :: TeamId
              -> ChannelId
              -> Session
              -> IO ()
mmJoinChannel teamid chanid sess = do
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
mmLeaveChannel :: TeamId
               -> ChannelId
               -> Session
               -> IO ()
mmLeaveChannel teamid chanid sess = do
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
mmGetPosts :: TeamId
           -> ChannelId
           -> Int -- offset in the backlog, 0 is most recent
           -> Int -- try to fetch this many
           -> Session
           -> IO Posts
mmGetPosts teamid chanid offset limit sess =
  mmDoRequest sess "mmGetPosts" $
  printf "/api/v3/teams/%s/channels/%s/posts/page/%d/%d"
         (idString teamid)
         (idString chanid)
         offset
         limit

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}\/posts\/since\/{utc_time}@
mmGetPostsSince :: TeamId
                -> ChannelId
                -> UTCTime
                -> Session
                -> IO Posts
mmGetPostsSince teamid chanid since sess =
  mmDoRequest sess "mmGetPostsSince" $
  printf "/api/v3/teams/%s/channels/%s/posts/since/%d"
         (idString teamid)
         (idString chanid)
         (utcTimeToMilliseconds since :: Int)

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}\/posts\/{post_id}\/get@
mmGetPost :: TeamId
          -> ChannelId
          -> PostId
          -> Session
          -> IO Posts
mmGetPost teamid chanid postid sess = do
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
mmGetPostsAfter :: TeamId
                -> ChannelId
                -> PostId
                -> Int -- offset in the backlog, 0 is most recent
                -> Int -- try to fetch this many
                -> Session
                -> IO Posts
mmGetPostsAfter teamid chanid postid offset limit sess =
  mmDoRequest sess "mmGetPosts" $
  printf "/api/v3/teams/%s/channels/%s/posts/%s/after/%d/%d"
         (idString teamid)
         (idString chanid)
         (idString postid)
         offset
         limit

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}\/posts\/{post_id}\/before\/{offset}\/{limit}@
mmGetPostsBefore :: TeamId
                 -> ChannelId
                 -> PostId
                 -> Int -- offset in the backlog, 0 is most recent
                 -> Int -- try to fetch this many
                 -> Session
                 -> IO Posts
mmGetPostsBefore teamid chanid postid offset limit sess =
  mmDoRequest sess "mmGetPosts" $
  printf "/api/v3/teams/%s/channels/%s/posts/%s/before/%d/%d"
         (idString teamid)
         (idString chanid)
         (idString postid)
         offset
         limit

-- |
-- route: @\/api\/v3\/files\/{file_id}\/get_info@
mmGetFileInfo :: FileId
              -> Session
              -> IO FileInfo
mmGetFileInfo fileId sess =
  mmDoRequest sess "mmGetFileInfo" $
  printf "/api/v3/files/%s/get_info" (idString fileId)

-- |
-- route: @\/api\/v4\/files\/{file_id}@
mmGetFile :: FileId
          -> Session
          -> IO B.ByteString
mmGetFile fileId sess@(Session cd _) = do
  let path = printf "/api/v4/files/%s" (idString fileId)
  uri  <- mmPath path
  runLogger cd "mmGetFile" $
    HttpRequest GET path Nothing
  rsp  <- mmRequest sess uri
  return (B.pack (rspBody rsp))

-- |
-- route: @\/api\/v3\/users\/{user_id}\/get@
mmGetUser :: UserId -> Session -> IO User
mmGetUser userid sess = mmDoRequest sess "mmGetUser" $
  printf "/api/v3/users/%s/get" (idString userid)

-- |
-- route: @\/api\/v3\/users\/{offset}\/{limit}@
mmGetUsers :: Int -> Int -> Session -> IO (HashMap UserId User)
mmGetUsers offset limit sess =
  mmDoRequest sess "mmGetUsers" $
    printf "/api/v3/users/%d/%d" offset limit

-- |
-- route: @\/api\/v3\/teams\/members\/{team_id}@
mmGetTeamMembers :: TeamId -> Session -> IO (Seq.Seq TeamMember)
mmGetTeamMembers teamid sess = mmDoRequest sess "mmGetTeamMembers" $
  printf "/api/v3/teams/members/%s" (idString teamid)

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/{channel_id}\/users\/{offset}\/{limit}@
mmGetChannelMembers :: TeamId
                    -> ChannelId
                    -> Int
                    -> Int
                    -> Session
                    -> IO (HashMap UserId User)
mmGetChannelMembers teamid chanid offset limit sess = mmDoRequest sess "mmGetChannelMembers" $
  printf "/api/v3/teams/%s/channels/%s/users/%d/%d"
         (idString teamid)
         (idString chanid)
         offset
         limit

-- |
-- route: @\/api\/v3\/users\/profiles_for_dm_list\/{team_id}@
mmGetProfilesForDMList :: TeamId
                       -> Session
                       -> IO (HashMap UserId User)
mmGetProfilesForDMList teamid sess =
  mmDoRequest sess "mmGetProfilesForDMList" $
    printf "/api/v3/users/profiles_for_dm_list/%s" (idString teamid)

-- |
-- route: @\/api\/v3\/users\/me@
mmGetMe :: Session -> IO User
mmGetMe sess = mmDoRequest sess "mmGetMe" "/api/v3/users/me"

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/users\/{offset}\/{limit}@
mmGetProfiles :: TeamId
              -> Int
              -> Int
              -> Session
              -> IO (HashMap UserId User)
mmGetProfiles teamid offset limit sess = mmDoRequest sess "mmGetProfiles" $
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
mmCreateDirect :: TeamId -> UserId -> Session -> IO Channel
mmCreateDirect teamid userid sess = do
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
mmCreateChannel :: TeamId -> MinChannel -> Session -> IO Channel
mmCreateChannel teamid payload sess = do
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
mmDeleteChannel :: TeamId -> ChannelId -> Session -> IO ()
mmDeleteChannel teamid chanid sess = do
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
mmDeletePost :: TeamId
             -> ChannelId
             -> PostId
             -> Session
             -> IO ()
mmDeletePost teamid chanid postid sess = do
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
mmUpdatePost :: TeamId
             -> Post
             -> Session
             -> IO Post
mmUpdatePost teamid post sess = do
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
mmPost :: TeamId
       -> PendingPost
       -> Session
       -> IO Post
mmPost teamid post sess = do
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

mmSaveConfig :: Value
             -> Session
             -> IO ()
mmSaveConfig config sess = do
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
mmChannelAddUser :: TeamId
                 -> ChannelId
                 -> UserId
                 -> Session
                 -> IO ChannelData
mmChannelAddUser teamid chanId uId sess = do
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
mmTeamAddUser :: TeamId
              -> UserId
              -> Session
              -> IO ()
mmTeamAddUser teamid uId sess = do
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
mmExecute :: TeamId
          -> MinCommand
          -> Session
          -> IO CommandResponse
mmExecute teamid command sess = do
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
mmUsersCreateWithSession :: UsersCreate
                         -> Session
                         -> IO User
mmUsersCreateWithSession usersCreate sess = do
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
mmGetReactionsForPost :: TeamId
                      -> ChannelId
                      -> PostId
                      -> Session
                      -> IO [Reaction]
mmGetReactionsForPost tId cId pId sess = do
  let path = printf "/api/v3/teams/%s/channels/%s/posts/%s/reactions"
                    (idString tId)
                    (idString cId)
                    (idString pId)
  mmDoRequest sess "mmGetReactionsForPost" path

-- |
-- route: @\/api\/v3\/preferences\/save@
mmSavePreferences :: Seq.Seq Preference
                  -> Session
                  -> IO ()
mmSavePreferences pref sess = do
  uri <- mmPath "/api/v3/preferences/save"
  _ <- mmPOST sess uri pref
  return ()

-- |
-- route: @\/api\/v3\/preferences\/save@
mmDeletePreferences :: Seq.Seq Preference
                    -> Session
                    -> IO ()
mmDeletePreferences pref sess = do
  uri <- mmPath "/api/v3/preferences/delete"
  _ <- mmPOST sess uri pref
  return ()

-- |
-- route: @\/api\/v3\/preferences\/save@
--
-- This is a convenience function for a particular use of
-- 'mmSavePreference'
mmFlagPost :: UserId
           -> PostId
           -> Session
           -> IO ()
mmFlagPost uId pId sess = do
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
mmUnflagPost :: UserId
             -> PostId
             -> Session
             -> IO ()
mmUnflagPost uId pId sess = do
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

mmGetFlaggedPosts :: UserId
                  -> Session
                  -> IO Posts
mmGetFlaggedPosts uId sess =
  let path = printf "/api/v4/users/%s/posts/flagged" (idString uId)
  in mmDoRequest sess "mmGetFlaggedPosts" path

mmGetMyPreferences :: Session
                -> IO (Seq.Seq Preference)
mmGetMyPreferences sess =
  mmDoRequest sess "mmMyPreferences" "/api/v4/users/me/preferences"


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

-- |
-- route: @\/api\/v3\/teams\/{team_id}\/channels\/update_header@
mmSetChannelHeader :: TeamId -> ChannelId -> T.Text -> Session -> IO Channel
mmSetChannelHeader teamid chanid header sess = do
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
