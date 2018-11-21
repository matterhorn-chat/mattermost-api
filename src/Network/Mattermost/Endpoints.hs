{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Network.Mattermost.Endpoints where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as HM
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Base as HTTP
import qualified Network.HTTP.Headers as HTTP
import           Text.Printf (printf)

import Network.Mattermost.Connection
import Network.Mattermost.Exceptions
import Network.Mattermost.Types
import Network.Mattermost.Types.Config
import Network.Mattermost.Types.Internal


mmLogin :: ConnectionData -> Login -> IO (Either LoginFailureException (Session, User))
mmLogin cd login = do
  rsp <- doUnauthRequest cd HTTP.POST "/users/login" (jsonBody login)
  case HTTP.rspCode rsp of
    (2, _, _) -> do
      token <- mmGetHeader rsp (HTTP.HdrCustom "Token")
      value <- mmGetJSONBody "User" rsp
      return (Right (Session cd (Token token), value))
    _ ->
      let eMsg = "Server returned unexpected " ++ show (HTTP.rspCode rsp) ++ " response"
      in return (Left (LoginFailureException eMsg))

mmInitialUser :: ConnectionData -> UsersCreate -> IO User
mmInitialUser cd users = do
  rsp <- doUnauthRequest cd HTTP.POST "/users" (jsonBody users)
  case HTTP.rspCode rsp of
    (2, _, _) -> mmGetJSONBody "User" rsp
    _ -> error ("Server returned unexpected " ++ show (HTTP.rspCode rsp) ++ " response")

-- * Endpoints

-- * Brand

-- -- | Uploads a brand image.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmUploadBrandImage :: Session -> IO ()
-- mmUploadBrandImage =
--   inPost "/brand/image" noBody jsonResponse

-- -- | Get the previously uploaded brand image. Returns 404 if no brand image
-- --   has been uploaded.
-- --
-- --   /Permissions/: No permission required.
-- mmGetBrandImage :: Session -> IO Text
-- mmGetBrandImage =
--   inGet "/brand/image" noBody jsonResponse



-- * Channels

-- | Get all channel members on a team for a user.
--
--   /Permissions/: Logged in as the user and @view_team@ permission for
--   the team. Having @manage_system@ permission voids the previous
--   requirements.
mmGetChannelMembersForUser :: UserParam -> TeamId -> Session -> IO (Seq ChannelMember)
mmGetChannelMembersForUser userId teamId =
  inGet (printf "/users/%s/teams/%s/channels/members" userId teamId) noBody jsonResponse

-- | Get all the channels on a team for a user.
--
--   /Permissions/: Logged in as the user, or have @edit_other_users@
--   permission, and @view_team@ permission for the team.
mmGetChannelsForUser :: UserParam -> TeamId -> Session -> IO (Seq Channel)
mmGetChannelsForUser userId teamId =
  inGet (printf "/users/%s/teams/%s/channels" userId teamId) noBody jsonResponse

-- -- | Get a list of channel members based on the provided user ids.
-- --
-- --   /Permissions/: Must have the @read_channel@ permission.
-- mmGetChannelMembersByIds :: ChannelId -> (Seq Text) -> Session -> IO (Seq ChannelMember)
-- mmGetChannelMembersByIds channelId body =
--   inPost (printf "/channels/%s/members/ids" channelId) (jsonBody body) jsonResponse

-- | Perform all the actions involved in viewing a channel. This includes
--   marking channels as read, clearing push notifications, and updating
--   the active channel.
--
--   /Permissions/: Must be logged in as user or have @edit_other_users@
--   permission.
mmViewChannel :: UserParam -> ChannelId -> Maybe ChannelId -> Session -> IO ()
mmViewChannel userId chanId prevChanIdMb =
  inPost (printf "/channels/members/%s/view" userId) (jsonBody body) noResponse
  where body = HM.fromList $
          ("channel_id" :: T.Text, chanId)
          : case prevChanIdMb of
              Just prevChanId -> [ ("prev_channel_id", prevChanId) ]
              Nothing         -> []

-- | Create a new group message channel to group of users. If the logged in
--   user's id is not included in the list, it will be appended to the end.
--
--   /Permissions/: Must have @create_group_channel@ permission.
mmCreateGroupMessageChannel :: Seq UserId -> Session -> IO Channel
mmCreateGroupMessageChannel body =
  inPost "/channels/group" (jsonBody body) jsonResponse

-- | Get the total unread messages and mentions for a channel for a user.
--
--   /Permissions/: Must be logged in as user and have the @read_channel@
--   permission, or have @edit_other_usrs@ permission.
mmGetUnreadMessages :: UserParam -> ChannelId -> Session -> IO ChannelUnread
mmGetUnreadMessages userId channelId =
  inGet (printf "/users/%s/channels/%s/unread" userId channelId) noBody jsonResponse

-- -- | Gets a channel from the provided team name and channel name strings.
-- --
-- --   /Permissions/: @read_channel@ permission for the channel.
-- mmGetChannelByNameAndTeamName :: Text -> Text -> Session -> IO Channel
-- mmGetChannelByNameAndTeamName teamName channelName =
--   inGet (printf "/teams/name/%s/channels/name/%s" teamName channelName) noBody jsonResponse

-- | Get a list of public channels on a team by id.
--
--   /Permissions/: @view_team@ for the team the channels are on.
mmGetListOfChannelsByIds :: TeamId -> Seq ChannelId -> Session -> IO (Seq Channel)
mmGetListOfChannelsByIds teamId body =
  inPost (printf "/teams/%s/channels/ids" teamId) (jsonBody body) jsonResponse

-- | Partially update a channel by providing only the fields you want to
--   update. Omitted fields will not be updated. The fields that can be
--   updated are defined in the request body, all other provided fields
--   will be ignored.
--
--   /Permissions/: If updating a public channel,
--   @manage_public_channel_members@ permission is required. If updating a
--   private channel, @manage_private_channel_members@ permission is
--   required.
mmPatchChannel :: ChannelId -> ChannelPatch -> Session -> IO Channel
mmPatchChannel channelId body =
  inPut (printf "/channels/%s/patch" channelId) (jsonBody body) jsonResponse

-- | Create a new direct message channel between two users.
--
--   /Permissions/: Must be one of the two users and have
--   @create_direct_channel@ permission. Having the @manage_system@
--   permission voids the previous requirements.
mmCreateDirectMessageChannel :: (UserId, UserId) -> Session -> IO Channel
mmCreateDirectMessageChannel body =
  inPost "/channels/direct" (jsonBody body) jsonResponse

-- -- | Get a list of pinned posts for channel.
-- mmGetChannelsPinnedPosts :: ChannelId -> Session -> IO PostList
-- mmGetChannelsPinnedPosts channelId =
--   inGet (printf "/channels/%s/pinned" channelId) noBody jsonResponse

-- | Get statistics for a channel.
--
--   /Permissions/: Must have the @read_channel@ permission.
mmGetChannelStatistics :: ChannelId -> Session -> IO ChannelStats
mmGetChannelStatistics channelId =
  inGet (printf "/channels/%s/stats" channelId) noBody jsonResponse

-- -- | Update a user's notification properties for a channel. Only the
-- --   provided fields are updated.
-- --
-- --   /Permissions/: Must be logged in as the user or have
-- --   @edit_other_users@ permission.
-- mmUpdateChannelNotifications :: ChannelId -> UserId -> XX17 -> Session -> IO ()
-- mmUpdateChannelNotifications channelId userId body =
--   inPut (printf "/channels/%s/members/%s/notify_props" channelId userId) (jsonBody body) jsonResponse

-- | Add a user to the channel.
mmAddUser :: ChannelId -> MinChannelMember -> Session -> IO ChannelMember
mmAddUser channelId body =
  inPost (printf "/channels/%s/members" channelId) (jsonBody body) jsonResponse

-- -- | Get a page of members for a channel.
-- --
-- --   /Permissions/: @read_channel@ permission for the channel.
-- mmGetChannelMembers :: ChannelId -> Maybe Integer -> Maybe Integer -> Session -> IO (Seq ChannelMember)
-- mmGetChannelMembers channelId page perPage =
--   inGet (printf "/channels/%s/members?%s" channelId (mkQueryString [ sequence ("page", fmap show page) , sequence ("per_page", fmap show perPage) ])) noBody jsonResponse

-- | Create a new channel.
--
--   /Permissions/: If creating a public channel, @create_public_channel@
--   permission is required. If creating a private channel,
--   @create_private_channel@ permission is required.
mmCreateChannel :: MinChannel -> Session -> IO Channel
mmCreateChannel body =
  inPost "/channels" (jsonBody body) jsonResponse

-- | Gets channel from the provided team id and channel name strings.
--
--   /Permissions/: @read_channel@ permission for the channel.
mmGetChannelByName :: TeamId -> Text -> Session -> IO Channel
mmGetChannelByName teamId channelName =
  inGet (printf "/teams/%s/channels/name/%s" teamId channelName) noBody jsonResponse

-- -- | Update a user's roles for a channel.
-- --
-- --   /Permissions/: Must have @manage_channel_roles@ permission for the
-- --   channel.
-- mmUpdateChannelRoles :: ChannelId -> UserId -> Text -> Session -> IO ()
-- mmUpdateChannelRoles channelId userId roles =
--   inPut (printf "/channels/%s/members/%s/roles" channelId userId) (jsonBody (A.object [ "roles" A..= roles ])) jsonResponse

-- -- | Update a channel. The fields that can be updated are listed as
-- --   paramters. Omitted fields will be treated as blanks.
-- --
-- --   /Permissions/: If updating a public channel,
-- --   @manage_public_channel_members@ permission is required. If updating a
-- --   private channel, @manage_private_channel_members@ permission is
-- --   required.
-- mmUpdateChannel :: ChannelId -> XX28 -> Session -> IO Channel
-- mmUpdateChannel channelId body =
--   inPut (printf "/channels/%s" channelId) (jsonBody body) jsonResponse

-- | Get channel from the provided channel id string.
--
--   /Permissions/: @read_channel@ permission for the channel.
mmGetChannel :: ChannelId -> Session -> IO Channel
mmGetChannel channelId =
  inGet (printf "/channels/%s" channelId) noBody jsonResponse

-- | Delete a channel based from provided channel id string.
--
--   /Permissions/: @delete_public_channel@ permission if the channel is
--   public,
--
--   @delete_private_channel@ permission if the channel is private,
--
--   or have @manage_system@ permission.
mmDeleteChannel :: ChannelId -> Session -> IO ()
mmDeleteChannel channelId =
  inDelete (printf "/channels/%s" channelId) noBody noResponse

-- -- | Restore channel from the provided channel id string.
-- --
-- --
-- --   /Minimum server version/: 3.10
-- --
-- --
-- --   /Permissions/: @manage_team@ permission for the team of channel.
-- mmRestoreChannel :: ChannelId -> Session -> IO Channel
-- mmRestoreChannel channelId =
--   inPost (printf "/channels/%s/restore" channelId) noBody jsonResponse

-- | Get a channel member.
--
--   /Permissions/: @read_channel@ permission for the channel.
mmGetChannelMember :: ChannelId -> UserParam -> Session -> IO ChannelMember
mmGetChannelMember channelId userId =
  inGet (printf "/channels/%s/members/%s" channelId userId) noBody jsonResponse

-- | Delete a channel member, effectively removing them from a channel.
--
--   /Permissions/: @manage_public_channel_members@ permission if the
--   channel is public.
--
--   @manage_private_channel_members@ permission if the channel is private.
mmRemoveUserFromChannel :: ChannelId -> UserParam -> Session -> IO ()
mmRemoveUserFromChannel channelId userId =
  inDelete (printf "/channels/%s/members/%s" channelId userId) noBody noResponse



-- * Cluster

-- -- | Get a set of information for each node in the cluster, useful for
-- --   checking the status and health of each node.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmGetClusterStatus :: Session -> IO (Seq ClusterInfo)
-- mmGetClusterStatus =
--   inGet "/cluster/status" noBody jsonResponse



-- * Commands

-- -- | Generate a new token for the command based on command id string.
-- --
-- --   /Permissions/: Must have @manage_slash_commands@ permission for the
-- --   team the command is in.
-- mmGenerateNewToken :: CommandId -> Session -> IO Text
-- mmGenerateNewToken commandId =
--   inPut (printf "/commands/%s/regen_token" commandId) noBody jsonResponse

-- | Execute a command on a team.
--
--   /Permissions/: Must have @use_slash_commands@ permission for the team
--   the command is in.
mmExecuteCommand :: MinCommand -> Session -> IO CommandResponse
mmExecuteCommand body =
  inPost "/commands/execute" (jsonBody body) jsonResponse

-- -- | Update a single command based on command id string and Command struct.
-- --
-- --   /Permissions/: Must have @manage_slash_commands@ permission for the
-- --   team the command is in.
-- mmUpdateCommand :: CommandId -> Command -> Session -> IO Command
-- mmUpdateCommand commandId body =
--   inPut (printf "/commands/%s" commandId) (jsonBody body) jsonResponse

-- -- | Delete a command based on command id string.
-- --
-- --   /Permissions/: Must have @manage_slash_commands@ permission for the
-- --   team the command is in.
-- mmDeleteCommand :: CommandId -> Session -> IO ()
-- mmDeleteCommand commandId =
--   inDelete (printf "/commands/%s" commandId) noBody jsonResponse

-- -- | List autocomplete commands in the team.
-- --
-- --   /Permissions/: @view_team@ for the team.
-- mmListAutocompleteCommands :: TeamId -> Session -> IO (Seq Command)
-- mmListAutocompleteCommands teamId =
--   inGet (printf "/teams/%s/commands/autocomplete" teamId) noBody jsonResponse

-- -- | Create a command for a team.
-- --
-- --   /Permissions/: @manage_slash_commands@ for the team the command is in.
-- mmCreateCommand :: XX32 -> Session -> IO Command
-- mmCreateCommand body =
--   inPost "/commands" (jsonBody body) jsonResponse

-- -- | List commands for a team.
-- --
-- --   /Permissions/: @manage_slash_commands@ if need list custom commands.
-- mmListCommandsForTeam :: TeamId -> Maybe Text -> Session -> IO (Seq Command)
-- mmListCommandsForTeam teamId customOnly =
--   inGet (printf "/commands?%s" (mkQueryString [ Just ("team_id", T.unpack (idString teamId)) , sequence ("custom_only", fmap T.unpack customOnly) ])) noBody jsonResponse



-- * Compliance

-- -- | Get a compliance reports previously created.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmGetReport :: ReportId -> Session -> IO Compliance
-- mmGetReport reportId =
--   inGet (printf "/compliance/reports/%s" reportId) noBody jsonResponse

-- -- | Create and save a compliance report.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmCreateReport :: Session -> IO Compliance
-- mmCreateReport =
--   inPost "/compliance/reports" noBody jsonResponse

-- -- | Get a list of compliance reports previously created by page, selected
-- --   with @page@ and @per_page@ query parameters.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmGetReports :: Maybe Integer -> Maybe Integer -> Session -> IO (Seq Compliance)
-- mmGetReports page perPage =
--   inGet (printf "/compliance/reports?%s" (mkQueryString [ sequence ("page", fmap show page) , sequence ("per_page", fmap show perPage) ])) noBody jsonResponse

-- -- | Download the full contents of a report as a file.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmDownloadReport :: ReportId -> Session -> IO ()
-- mmDownloadReport reportId =
--   inGet (printf "/compliance/reports/%s/download" reportId) noBody jsonResponse



-- * Elasticsearch

-- -- | Deletes all Elasticsearch indexes and their contents. After calling
-- --   this endpoint, it is
-- --
-- --   necessary to schedule a new Elasticsearch indexing job to repopulate
-- --   the indexes.
-- --
-- --   /Minimum server version/: 4.1
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmPurgeAllElasticsearchIndexes :: Session -> IO ()
-- mmPurgeAllElasticsearchIndexes =
--   inPost "/elasticsearch/purge_indexes" noBody jsonResponse

-- -- | Test the current Elasticsearch configuration to see if the
-- --   Elasticsearch server can be contacted successfully.
-- --
-- --   Optionally provide a configuration in the request body to test. If no
-- --   valid configuration is present in the
-- --
-- --   request body the current server configuration will be tested.
-- --
-- --
-- --   /Minimum server version/: 4.1
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmTestElasticsearchConfiguration :: Session -> IO ()
-- mmTestElasticsearchConfiguration =
--   inPost "/elasticsearch/test" noBody jsonResponse



-- * Emoji

-- -- | Create a custom emoji for the team.
-- --
-- --   /Permissions/: Must be authenticated.
-- mmCreateCustomEmoji :: Session -> IO Emoji
-- mmCreateCustomEmoji =
--   inPost "/emoji" noBody jsonResponse

-- -- | Get a page of metadata for custom emoji on the system.
-- --
-- --   /Permissions/: Must be authenticated.
-- mmGetListOfCustomEmoji :: Maybe Integer -> Maybe Integer -> Session -> IO Emoji
-- mmGetListOfCustomEmoji page perPage =
--   inGet (printf "/emoji?%s" (mkQueryString [ sequence ("page", fmap show page) , sequence ("per_page", fmap show perPage) ])) noBody jsonResponse

-- -- | Get some metadata for a custom emoji.
-- --
-- --   /Permissions/: Must be authenticated.
-- mmGetCustomEmoji :: EmojiId -> Session -> IO Emoji
-- mmGetCustomEmoji emojiId =
--   inGet (printf "/emoji/%s" emojiId) noBody jsonResponse

-- -- | Delete a custom emoji.
-- --
-- --   /Permissions/: Must have the @manage_team@ or @manage_system@
-- --   permissions or be the user who created the emoji.
-- mmDeleteCustomEmoji :: EmojiId -> Session -> IO Emoji
-- mmDeleteCustomEmoji emojiId =
--   inDelete (printf "/emoji/%s" emojiId) noBody jsonResponse

-- -- | Get the image for a custom emoji.
-- --
-- --   /Permissions/: Must be authenticated.
-- mmGetCustomEmojiImage :: EmojiId -> Session -> IO ()
-- mmGetCustomEmojiImage emojiId =
--   inGet (printf "/emoji/%s/image" emojiId) noBody jsonResponse



-- * Files

-- -- | Gets a public link for a file that can be accessed without logging
-- mmGetPublicFileLink :: FileId -> Session -> IO Text
-- mmGetPublicFileLink fileId =
--   inGet (printf "/files/%s/link" fileId) noBody jsonResponse

-- | Gets a file that has been uploaded previously.
--
--   /Permissions/: Must have @read_channel@ permission or be uploader of
--   the file.
mmGetFile :: FileId -> Session -> IO B.ByteString
mmGetFile fileId =
  inGet (printf "/files/%s" fileId) noBody bytestringResponse

-- -- | Uploads a file that can later be attached to a post.
-- --
-- --   /Permissions/: Must have @upload_file@ permission.
-- mmUploadFile :: Session -> IO XX15
-- mmUploadFile =
--   inPost "/files" noBody jsonResponse

-- | Gets a file's info.
--
--   /Permissions/: Must have @read_channel@ permission or be uploader of
--   the file.
mmGetMetadataForFile :: FileId -> Session -> IO FileInfo
mmGetMetadataForFile fileId =
  inGet (printf "/files/%s/info" fileId) noBody jsonResponse

-- -- | Gets a file's thumbnail.
-- --
-- --   /Permissions/: Must have @read_channel@ permission or be uploader of
-- --   the file.
-- mmGetFilesThumbnail :: FileId -> Session -> IO ()
-- mmGetFilesThumbnail fileId =
--   inGet (printf "/files/%s/thumbnail" fileId) noBody jsonResponse

-- -- | Gets a file's preview.
-- --
-- --   /Permissions/: Must have @read_channel@ permission or be uploader of
-- --   the file.
-- mmGetFilesPreview :: FileId -> Session -> IO ()
-- mmGetFilesPreview fileId =
--   inGet (printf "/files/%s/preview" fileId) noBody jsonResponse



-- * Jobs

-- -- | Create a new job.
-- --
-- --   /Minimum server version: 4.1/
-- --
-- --   /Permissions/: Must have @manage_jobs@ permission.
-- mmCreateNewJob :: XX20 -> Session -> IO Job
-- mmCreateNewJob body =
--   inPost "/jobs" (jsonBody body) jsonResponse

-- -- | Get a page of jobs. Use the query parameters to modify the behaviour
-- --   of this endpoint.
-- --
-- --   /Minimum server version: 4.1/
-- --
-- --   /Permissions/: Must have @manage_jobs@ permission.
-- mmGetJobs :: Maybe Integer -> Maybe Integer -> Session -> IO (Seq Job)
-- mmGetJobs page perPage =
--   inGet (printf "/jobs?%s" (mkQueryString [ sequence ("page", fmap show page) , sequence ("per_page", fmap show perPage) ])) noBody jsonResponse

-- -- | Cancel a job.
-- --
-- --   /Minimum server version: 4.1/
-- --
-- --   /Permissions/: Must have @manage_jobs@ permission.
-- mmCancelJob :: JobId -> Session -> IO ()
-- mmCancelJob jobId =
--   inPost (printf "/jobs/%s/cancel" jobId) noBody jsonResponse

-- -- | Gets a single job.
-- --
-- --   /Minimum server version: 4.1/
-- --
-- --   /Permissions/: Must have @manage_jobs@ permission.
-- mmGetJob :: JobId -> Session -> IO Job
-- mmGetJob jobId =
--   inGet (printf "/jobs/%s" jobId) noBody jsonResponse

-- -- | Get a page of jobs of the given type. Use the query parameters to
-- --   modify the behaviour of this endpoint.
-- --
-- --   /Minimum server version: 4.1/
-- --
-- --   /Permissions/: Must have @manage_jobs@ permission.
-- mmGetJobsOfGivenType :: Text -> Maybe Integer -> Maybe Integer -> Session -> IO (Seq Job)
-- mmGetJobsOfGivenType type_ page perPage =
--   inGet (printf "/jobs/type/%s?%s" type_ (mkQueryString [ sequence ("page", fmap show page) , sequence ("per_page", fmap show perPage) ])) noBody jsonResponse



-- * LDAP

-- -- | Test the current AD\/LDAP configuration to see if the AD\/LDAP server
-- --   can be contacted successfully.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmTestLdapConfiguration :: Session -> IO ()
-- mmTestLdapConfiguration =
--   inPost "/ldap/test" noBody jsonResponse

-- -- | Synchronize any user attribute changes in the configured AD\/LDAP
-- --   server with Mattermost.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmSyncWithLdap :: Session -> IO ()
-- mmSyncWithLdap =
--   inPost "/ldap/sync" noBody jsonResponse



-- * OAuth

-- -- | Register an OAuth 2.0 client application with Mattermost as the
-- --   service provider.
-- --
-- --   /Permissions/: Must have @manage_oauth@ permission.
-- mmRegisterOauthApp :: XX13 -> Session -> IO OAuthApp
-- mmRegisterOauthApp body =
--   inPost "/oauth/apps" (jsonBody body) jsonResponse

-- -- | Get a page of OAuth 2.0 client applications registered with
-- --   Mattermost.
-- --
-- --   /Permissions/: With @manage_oauth@ permission, the apps registered by
-- --   the logged in user are returned. With @manage_system_wide_oauth@
-- --   permission, all apps regardless of creator are returned.
-- mmGetOauthApps :: Maybe Integer -> Maybe Integer -> Session -> IO (Seq OAuthApp)
-- mmGetOauthApps page perPage =
--   inGet (printf "/oauth/apps?%s" (mkQueryString [ sequence ("page", fmap show page) , sequence ("per_page", fmap show perPage) ])) noBody jsonResponse

-- -- | Get an OAuth 2.0 client application registered with Mattermost.
-- --
-- --   /Permissions/: If app creator, must have @mange_oauth@ permission
-- --   otherwise @manage_system_wide_oauth@ permission is required.
-- mmGetAnOauthApp :: AppId -> Session -> IO OAuthApp
-- mmGetAnOauthApp appId =
--   inGet (printf "/oauth/apps/%s" appId) noBody jsonResponse

-- -- | Delete and unregister an OAuth 2.0 client application
-- --
-- --   /Permissions/: If app creator, must have @mange_oauth@ permission
-- --   otherwise @manage_system_wide_oauth@ permission is required.
-- mmDeleteAnOauthApp :: AppId -> Session -> IO ()
-- mmDeleteAnOauthApp appId =
--   inDelete (printf "/oauth/apps/%s" appId) noBody jsonResponse

-- -- | Get public information about an OAuth 2.0 client application
-- --   registered with Mattermost. The application's client secret will be
-- --   blanked out.
-- --
-- --   /Permissions/: Must be authenticated.
-- mmGetInfoOnAnOauthApp :: AppId -> Session -> IO OAuthApp
-- mmGetInfoOnAnOauthApp appId =
--   inGet (printf "/oauth/apps/%s/info" appId) noBody jsonResponse

-- -- | Get a page of OAuth 2.0 client applications authorized to access a
-- --   user's account.
-- --
-- --   /Permissions/: Must be authenticated as the user or have
-- --   @edit_other_users@ permission.
-- mmGetAuthorizedOauthApps :: UserId -> Maybe Integer -> Maybe Integer -> Session -> IO (Seq OAuthApp)
-- mmGetAuthorizedOauthApps userId page perPage =
--   inGet (printf "/users/%s/oauth/apps/authorized?%s" userId (mkQueryString [ sequence ("page", fmap show page) , sequence ("per_page", fmap show perPage) ])) noBody jsonResponse

-- -- | Regenerate the client secret for an OAuth 2.0 client application
-- --   registered with Mattermost.
-- --
-- --   /Permissions/: If app creator, must have @mange_oauth@ permission
-- --   otherwise @manage_system_wide_oauth@ permission is required.
-- mmRegenerateOauthAppSecret :: AppId -> Session -> IO OAuthApp
-- mmRegenerateOauthAppSecret appId =
--   inPost (printf "/oauth/apps/%s/regen_secret" appId) noBody jsonResponse



-- * Posts

-- | Create a new post in a channel. To create the post as a comment on
--   another post, provide @root_id@.
--
--   /Permissions/: Must have @create_post@ permission for the channel the
--   post is being created in.
mmCreatePost :: RawPost -> Session -> IO Post
mmCreatePost body =
  inPost "/posts" (jsonBody body) jsonResponse

-- | Search posts in the team and from the provided terms string.
--
--   /Permissions/: Must be authenticated and have the @view_team@
--   permission.
mmSearchForTeamPosts :: TeamId -> SearchPosts -> Session -> IO Posts
mmSearchForTeamPosts teamId body =
  inPost (printf "/teams/%s/posts/search" teamId) (jsonBody body) jsonResponse

-- -- | Pin a post to a channel it is in based from the provided post id
-- --   string.
-- --
-- --   /Permissions/: Must be authenticated and have the @read_channel@
-- --   permission to the channel the post is in.
-- mmPinPostToChannel :: PostId -> Session -> IO ()
-- mmPinPostToChannel postId =
--   inPost (printf "/posts/%s/pin" postId) noBody jsonResponse

-- | Get a post and the rest of the posts in the same thread.
--
--   /Permissions/: Must have @read_channel@ permission for the channel the
--   post is in or if the channel is public, have the
--   @read_public_channels@ permission for the team.
mmGetThread :: PostId -> Session -> IO Posts
mmGetThread postId =
  inGet (printf "/posts/%s/thread" postId) noBody jsonResponse

-- | Update a post. Only the fields listed below are updatable, omitted
--   fields will be treated as blank.
--
--   /Permissions/: Must have @edit_post@ permission for the channel the
--   post is in.
mmUpdatePost :: PostId -> PostUpdate -> Session -> IO Post
mmUpdatePost postId body =
  inPut (printf "/posts/%s" postId) (jsonBody body) jsonResponse

-- | Get a single post.
--
--   /Permissions/: Must have @read_channel@ permission for the channel the
--   post is in or if the channel is public, have the
--   @read_public_channels@ permission for the team.
mmGetPost :: PostId -> Session -> IO Post
mmGetPost postId =
  inGet (printf "/posts/%s" postId) noBody jsonResponse

-- | Soft deletes a post, by marking the post as deleted in the database.
--   Soft deleted posts will not be returned in post queries.
--
--   /Permissions/: Must be logged in as the user or have
--   @delete_others_posts@ permission.
mmDeletePost :: PostId -> Session -> IO ()
mmDeletePost postId =
  inDelete (printf "/posts/%s" postId) noBody noResponse

data FlaggedPostsQuery = FlaggedPostsQuery
  { flaggedPostsQueryPage      :: Maybe Int
  , flaggedPostsQueryPerPage   :: Maybe Int
  , flaggedPostsQueryTeamId    :: Maybe TeamId
  , flaggedPostsQueryChannelId :: Maybe ChannelId
  }

defaultFlaggedPostsQuery :: FlaggedPostsQuery
defaultFlaggedPostsQuery = FlaggedPostsQuery
  { flaggedPostsQueryPage      = Nothing
  , flaggedPostsQueryPerPage   = Nothing
  , flaggedPostsQueryTeamId    = Nothing
  , flaggedPostsQueryChannelId = Nothing
  }


-- | Get a page of flagged posts of a user provided user id string. Selects
--   from a channel, team or all flagged posts by a user.
--
--   /Permissions/: Must be user or have @manage_system@ permission.
mmGetListOfFlaggedPosts :: UserParam -> FlaggedPostsQuery -> Session -> IO Posts
mmGetListOfFlaggedPosts userId FlaggedPostsQuery { .. } =
  inGet (printf "/users/%s/posts/flagged?%s" userId query) noBody jsonResponse
    where query = mkQueryString
            [ sequence ("team_id", fmap (T.unpack . idString) flaggedPostsQueryTeamId)
            , sequence ("channel_id", fmap (T.unpack . idString) flaggedPostsQueryChannelId)
            , sequence ("page", fmap show flaggedPostsQueryPage)
            , sequence ("per_page", fmap show flaggedPostsQueryPerPage)
            ]

-- -- | Unpin a post to a channel it is in based from the provided post id
-- --   string.
-- --
-- --   /Permissions/: Must be authenticated and have the @read_channel@
-- --   permission to the channel the post is in.
-- mmUnpinPostToChannel :: PostId -> Session -> IO ()
-- mmUnpinPostToChannel postId =
--   inPost (printf "/posts/%s/unpin" postId) noBody jsonResponse

-- | Partially update a post by providing only the fields you want to
--   update. Omitted fields will not be updated. The fields that can be
--   updated are defined in the request body, all other provided fields
--   will be ignored.
--
--   /Permissions/: Must have the @edit_post@ permission.
mmPatchPost :: PostId -> PostUpdate -> Session -> IO Post
mmPatchPost postId body =
  inPut (printf "/posts/%s/patch" postId) (jsonBody body) jsonResponse

data PostQuery = PostQuery
  { postQueryPage    :: Maybe Int
  , postQueryPerPage :: Maybe Int
  , postQuerySince   :: Maybe ServerTime
  , postQueryBefore  :: Maybe PostId
  , postQueryAfter   :: Maybe PostId
  }

defaultPostQuery :: PostQuery
defaultPostQuery = PostQuery
  { postQueryPage    = Nothing
  , postQueryPerPage = Nothing
  , postQuerySince   = Nothing
  , postQueryBefore  = Nothing
  , postQueryAfter   = Nothing
  }

postQueryToQueryString :: PostQuery -> String
postQueryToQueryString PostQuery { .. } =
  mkQueryString
    [ sequence ("page", fmap show postQueryPage)
    , sequence ("per_page", fmap show postQueryPerPage)
    , sequence ("since", fmap (show . timeToServer) postQuerySince)
    , sequence ("before", fmap (T.unpack . idString) postQueryBefore)
    , sequence ("after", fmap (T.unpack . idString) postQueryAfter)
    ]

-- | Get a page of posts in a channel. Use the query parameters to modify
--   the behaviour of this endpoint. The parameters @since@, @before@ and
--   @after@ must not be used together.
--
--   /Permissions/: Must have @read_channel@ permission for the channel.
mmGetPostsForChannel :: ChannelId -> PostQuery -> Session -> IO Posts
mmGetPostsForChannel channelId postQuery =
  inGet (printf "/channels/%s/posts?%s" channelId (postQueryToQueryString postQuery)) noBody jsonResponse

-- -- | Gets a list of file information objects for the files attached to a
-- --   post.
-- --
-- --   /Permissions/: Must have @read_channel@ permission for the channel the
-- --   post is in.
-- mmGetFileInfoForPost :: PostId -> Session -> IO (Seq FileInfo)
-- mmGetFileInfoForPost postId =
--   inGet (printf "/posts/%s/files/info" postId) noBody jsonResponse



-- * Preferences

-- | Gets a single preference for the current user with the given category
--   and name.
--
--   /Permissions/: Must be logged in as the user being updated or have the
--   @edit_other_users@ permission.
mmGetSpecificUserPreference :: UserParam -> Text -> Text -> Session -> IO Preference
mmGetSpecificUserPreference userId category preferenceName =
  inGet (printf "/users/%s/preferences/%s/name/%s" userId category preferenceName) noBody jsonResponse

-- | Save a list of the user's preferences.
--
--   /Permissions/: Must be logged in as the user being updated or have the
--   @edit_other_users@ permission.
mmSaveUsersPreferences :: UserParam -> (Seq Preference) -> Session -> IO ()
mmSaveUsersPreferences userId body =
  inPut (printf "/users/%s/preferences" userId) (jsonBody body) noResponse

-- | Get a list of the user's preferences.
--
--   /Permissions/: Must be logged in as the user being updated or have the
--   @edit_other_users@ permission.
mmGetUsersPreferences :: UserParam -> Session -> IO (Seq Preference)
mmGetUsersPreferences userId =
  inGet (printf "/users/%s/preferences" userId) noBody jsonResponse

-- | Delete a list of the user's preferences.
--
--   /Permissions/: Must be logged in as the user being updated or have the
--   @edit_other_users@ permission.
mmDeleteUsersPreferences :: UserParam -> (Seq Preference) -> Session -> IO ()
mmDeleteUsersPreferences userId body =
  inPost (printf "/users/%s/preferences/delete" userId) (jsonBody body) noResponse

-- | Lists the current user's stored preferences in the given category.
--
--   /Permissions/: Must be logged in as the user being updated or have the
--   @edit_other_users@ permission.
mmListUsersPreferencesByCategory :: UserParam -> Text -> Session -> IO (Seq Preference)
mmListUsersPreferencesByCategory userId category =
  inGet (printf "/users/%s/preferences/%s" userId category) noBody jsonResponse


-- * Reactions

mmGetReactionsForPost :: PostId -> Session -> IO (Seq Reaction)
mmGetReactionsForPost postId =
  inGet (printf "/posts/%s/reactions" postId) noBody jsonResponse

-- mmPostReaction :: Session -> IO ()
-- mmPostReaction =
--   inPost (printf "/reactions") (jsonBody ()) noResponse



-- * SAML

-- -- | Upload the private key to be used for encryption with your SAML
-- --   configuration. This will also set the filename for the PrivateKeyFile
-- --   setting in your @config.json@.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmUploadPrivateKey :: Session -> IO ()
-- mmUploadPrivateKey =
--   inPost "/saml/certificate/private" noBody noResponse

-- -- | Delete the current private key being used with your SAML
-- --   configuration. This will also disable encryption for SAML on your
-- --   system as this key is required for that.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmRemovePrivateKey :: Session -> IO ()
-- mmRemovePrivateKey =
--   inDelete "/saml/certificate/private" noBody jsonResponse

-- -- | Upload the public certificate to be used for encryption with your SAML
-- --   configuration. This will also set the filename for the
-- --   PublicCertificateFile setting in your @config.json@.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmUploadPublicCertificate :: Session -> IO ()
-- mmUploadPublicCertificate =
--   inPost "/saml/certificate/public" noBody jsonResponse

-- -- | Delete the current public certificate being used with your SAML
-- --   configuration. This will also disable encryption for SAML on your
-- --   system as this certificate is required for that.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmRemovePublicCertificate :: Session -> IO ()
-- mmRemovePublicCertificate =
--   inDelete "/saml/certificate/public" noBody jsonResponse

-- -- | Upload the IDP certificate to be used with your SAML configuration.
-- --   This will also set the filename for the IdpCertificateFile setting in
-- --   your @config.json@.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmUploadIdpCertificate :: Session -> IO ()
-- mmUploadIdpCertificate =
--   inPost "/saml/certificate/idp" noBody jsonResponse

-- -- | Delete the current IDP certificate being used with your SAML
-- --   configuration. This will also disable SAML on your system as this
-- --   certificate is required for SAML.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmRemoveIdpCertificate :: Session -> IO ()
-- mmRemoveIdpCertificate =
--   inDelete "/saml/certificate/idp" noBody jsonResponse

-- -- | Get the status of the uploaded certificates and keys in use by your
-- --   SAML configuration.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmGetCertificateStatus :: Session -> IO SamlCertificateStatus
-- mmGetCertificateStatus =
--   inGet "/saml/certificate/status" noBody jsonResponse

-- -- | Get SAML metadata from the server. SAML must be configured properly.
-- --
-- --   /Permissions/: No permission required.
-- mmGetMetadata :: Session -> IO Text
-- mmGetMetadata =
--   inGet "/saml/metadata" noBody jsonResponse


-- * Statuses

mmGetUserStatusByIds :: Seq UserId -> Session -> IO (Seq Status)
mmGetUserStatusByIds body =
  inPost "/users/status/ids" (jsonBody body) jsonResponse


-- * System

-- -- | Get a page of audits for all users on the system, selected with @page@
-- --   and @per_page@ query parameters.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmGetAudits :: Maybe Integer -> Maybe Integer -> Session -> IO (Seq Audit)
-- mmGetAudits page perPage =
--   inGet (printf "/audits?%s" (mkQueryString [ sequence ("page", fmap show page) , sequence ("per_page", fmap show perPage) ])) noBody jsonResponse

-- -- | Get a subset of the server license needed by the client.
-- --
-- --   /Permissions/: No permission required but having the @manage_system@
-- --   permission returns more information.
-- mmGetClientLicense :: Text -> Session -> IO ()
-- mmGetClientLicense format =
--   inGet (printf "/license/client?%s" (mkQueryString [ Just ("format", T.unpack format) ])) noBody jsonResponse

-- -- | Upload a license to enable enterprise features.
-- --
-- --   /Minimum server version/: 4.0
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmUploadLicenseFile :: Session -> IO ()
-- mmUploadLicenseFile =
--   inPost "/license" noBody jsonResponse

-- -- | Remove the license file from the server. This will disable all
-- --   enterprise features.
-- --
-- --   /Minimum server version/: 4.0
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmRemoveLicenseFile :: Session -> IO ()
-- mmRemoveLicenseFile =
--   inDelete "/license" noBody jsonResponse

-- -- | Get a valid WebRTC token, STUN and TURN server URLs along with TURN
-- --   credentials to use with the Mattermost WebRTC service. See
-- --   https:\/\/docs.mattermost.com\/administration\/config-settings.html
-- --   #webrtc-beta for WebRTC configutation settings. The token returned is
-- --   for the current user's session.
-- --
-- --   /Permissions/: Must be authenticated.
-- mmGetWebrtcToken :: Session -> IO XX8
-- mmGetWebrtcToken =
--   inGet "/webrtc/token" noBody jsonResponse

-- | Get a subset of the server configuration needed by the client.
--
--   /Permissions/: No permission required.
mmGetClientConfiguration :: Maybe Text -> Session -> IO ClientConfig
mmGetClientConfiguration format =
  inGet (printf "/config/client?%s" (mkQueryString [ sequence ("format", fmap T.unpack format) ])) noBody jsonResponse

-- -- | Reload the configuration file to pick up on any changes made to it.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmReloadConfiguration :: Session -> IO ()
-- mmReloadConfiguration =
--   inPost "/config/reload" noBody jsonResponse

-- -- | Purge all the in-memory caches for the Mattermost server. This can
-- --   have a temporary negative effect on performance while the caches are
-- --   re-populated.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmInvalidateAllCaches :: Session -> IO ()
-- mmInvalidateAllCaches =
--   inPost "/caches/invalidate" noBody jsonResponse

-- -- | Recycle database connections by closing and reconnecting all
-- --   connections to master and read replica databases.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmRecycleDatabaseConnections :: Session -> IO ()
-- mmRecycleDatabaseConnections =
--   inPost "/database/recycle" noBody jsonResponse

-- -- | Add log messages to the server logs.
-- --
-- --   /Permissions/: Must have @manage_system@ permission or if
-- --   @ServiceSettings.EnableDeveloper@ in the
-- --
-- --   config file is set to @true@ then any user can use this endpoint.
-- mmAddLogMessage :: XX26 -> Session -> IO UnknownType
-- mmAddLogMessage body =
--   inPost "/logs" (jsonBody body) jsonResponse

-- -- | Get a page of server logs, selected with @page@ and @per_page@ query
-- --   parameters.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmGetLogs :: Maybe Integer -> Maybe Integer -> Session -> IO (Seq Text)
-- mmGetLogs page perPage =
--   inGet (printf "/logs?%s" (mkQueryString [ sequence ("page", fmap show page) , sequence ("per_page", fmap show perPage) ])) noBody jsonResponse

-- -- | Send a test email to make sure you have your email settings configured
-- --   correctly. Optionally provide a configuration in the request body to
-- --   test. If no valid configuration is present in the request body the
-- --   current server configuration will be tested.
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmSendTestEmail :: Session -> IO ()
-- mmSendTestEmail =
--   inPost "/email/test" noBody jsonResponse

-- -- | Check if the server is up and healthy based on the configuration
-- --   setting @GoRoutineHealthThreshold@. If @GoRoutineHealthThreshold@ and
-- --   the number of goroutines on the server exceeds that threshold the
-- --   server is considered unhealthy. If @GoRoutineHealthThreshold@ is not
-- --   set or the number of goroutines is below the threshold the server is
-- --   considered healthy.
-- --
-- --   /Minimum server version/: 3.10
-- --
-- --   /Permissions/: Must be logged in.
-- mmCheckSystemHealth :: Session -> IO UnknownType
-- mmCheckSystemHealth =
--   inGet "/system/ping" noBody jsonResponse

-- -- | Get some analytics data about the system. This endpoint uses the old
-- --   format, the @\/analytics@ route is reserved for the new format when it
-- --   gets implemented.
-- --
-- --
-- --   The returned JSON changes based on the @name@ query parameter but is
-- --   always key\/value pairs.
-- --
-- --
-- --   /Minimum server version/: 4.0
-- --
-- --
-- --   /Permissions/: Must have @manage_system@ permission.
-- mmGetAnalytics :: Maybe Text -> TeamId -> Session -> IO ()
-- mmGetAnalytics name teamId =
--   inGet (printf "/analytics/old?%s" (mkQueryString [ sequence ("name", fmap T.unpack name) , Just ("team_id", T.unpack (idString teamId)) ])) noBody jsonResponse

-- | Submit a new configuration for the server to use.
--
--   /Permissions/: Must have @manage_system@ permission.
mmUpdateConfiguration :: ServerConfig -> Session -> IO ServerConfig
mmUpdateConfiguration body =
  inPut "/config" (jsonBody body) jsonResponse

-- | Retrieve the current server configuration
--
--   /Permissions/: Must have @manage_system@ permission.
mmGetConfiguration :: Session -> IO ServerConfig
mmGetConfiguration =
  inGet "/config" noBody jsonResponse



-- * Teams

-- | Get a team member on the system.
--
--   /Permissions/: Must be authenticated and have the @view_team@
--   permission.
mmGetTeamMember :: TeamId -> UserParam -> Session -> IO TeamMember
mmGetTeamMember teamId userId =
  inGet (printf "/teams/%s/members/%s" teamId userId) noBody jsonResponse

-- | Delete the team member object for a user, effectively removing them
--   from a team.
--
--   /Permissions/: Must be logged in as the user or have the
--   @remove_user_from_team@ permission.
mmRemoveUserFromTeam :: TeamId -> UserParam -> Session -> IO ()
mmRemoveUserFromTeam teamId userId =
  inDelete (printf "/teams/%s/members/%s" teamId userId) noBody jsonResponse

-- -- | Get a page of deleted channels on a team based on query string
-- --   parameters - team_id, page and per_page.
-- --
-- --
-- --   /Minimum server version/: 3.10
-- --
-- --
-- --   /Permissions/: Must be authenticated and have the @manage_team@
-- --   permission.
-- mmGetDeletedChannels :: TeamId -> Maybe Integer -> Maybe Integer -> Session -> IO (Seq Channel)
-- mmGetDeletedChannels teamId page perPage =
--   inGet (printf "/teams/%s/channels/deleted?%s" teamId (mkQueryString [ sequence ("page", fmap show page) , sequence ("per_page", fmap show perPage) ])) noBody jsonResponse

-- -- | Update a team member roles. Valid team roles are "team_user",
-- --   "team_admin" or both of them. Overwrites any previously assigned team
-- --   roles.
-- --
-- --   /Permissions/: Must be authenticated and have the @manage_team_roles@
-- --   permission.
-- mmUpdateTeamMemberRoles :: TeamId -> UserId -> Text -> Session -> IO ()
-- mmUpdateTeamMemberRoles teamId userId roles =
--   inPut (printf "/teams/%s/members/%s/roles" teamId userId) (jsonBody (A.object [ "roles" A..= roles ])) jsonResponse

-- | Get a page of public channels on a team based on query string
--   parameters - page and per_page.
--
--   /Permissions/: Must be authenticated and have the @list_team_channels@
--   permission.
mmGetPublicChannels :: TeamId -> Maybe Int -> Maybe Int -> Session -> IO (Seq Channel)
mmGetPublicChannels teamId page perPage =
  inGet (printf "/teams/%s/channels?%s" teamId (mkQueryString [ sequence ("page", fmap show page)
                                                              , sequence ("per_page", fmap show perPage)
                                                              ])) noBody jsonResponse

-- -- | Check if the team exists based on a team name.
-- mmCheckIfTeamExists :: Text -> Session -> IO TeamExists
-- mmCheckIfTeamExists name =
--   inGet (printf "/teams/name/%s/exists" name) noBody jsonResponse

-- | Create a new team on the system.
--
--   /Permissions/: Must be authenticated and have the @create_team@
--   permission.
mmCreateTeam :: TeamsCreate -> Session -> IO Team
mmCreateTeam body =
  inPost "/teams" (jsonBody body) jsonResponse

-- | For regular users only returns open teams. Users with the
--   "manage_system" permission will return teams regardless of type. The
--   result is based on query string parameters - page and per_page.
--
--   /Permissions/: Must be authenticated. "manage_system" permission is
--   required to show all teams.
mmGetTeams :: Maybe Integer -> Maybe Integer -> Session -> IO (Seq Team)
mmGetTeams page perPage =
  inGet (printf "/teams?%s"
          (mkQueryString [ sequence ("page", fmap show page)
                         , sequence ("per_page", fmap show perPage)
                         ])) noBody jsonResponse

-- | Search teams based on search term provided in the request body.
--
--   /Permissions/: Logged in user only shows open teams
--
--   Logged in user with "manage_system" permission shows all teams
mmSearchTeams :: Text -> Session -> IO (Seq Team)
mmSearchTeams term =
  inPost "/teams/search" (jsonBody (A.object [ "term" A..= term ])) jsonResponse

-- -- | Invite users to the existing team usign the user's email.
-- --
-- --   /Permissions/: Must have @invite_to_team@ permission for the team.
-- mmInviteUsersToTeamByEmail :: TeamId -> (Seq Text) -> Session -> IO ()
-- mmInviteUsersToTeamByEmail teamId body =
--   inPost (printf "/teams/%s/invite/email" teamId) (jsonBody body) jsonResponse

-- | Search public channels on a team based on the search term provided in
--   the request body.
--
--   /Permissions/: Must have the @list_team_channels@ permission.
mmSearchChannels :: TeamId -> Text -> Session -> IO (Seq Channel)
mmSearchChannels teamId term =
  inPost (printf "/teams/%s/channels/search" teamId) (jsonBody (A.object [ "term" A..= term ])) jsonResponse

-- -- | Get the count for unread messages and mentions in the teams the user
-- --   is a member of.
-- --
-- --   /Permissions/: Must be logged in.
-- mmGetTeamUnreadsForUser :: UserId -> Text -> Session -> IO (Seq TeamUnread)
-- mmGetTeamUnreadsForUser userId excludeTeam =
--   inGet (printf "/users/%s/teams/unread?%s" userId (mkQueryString [ Just ("exclude_team", T.unpack excludeTeam) ])) noBody jsonResponse

-- | Get a list of teams that a user is on.
--
--   /Permissions/: Must be authenticated as the user or have the
--   @manage_system@ permission.
mmGetUsersTeams :: UserParam -> Session -> IO (Seq Team)
mmGetUsersTeams userId =
  inGet (printf "/users/%s/teams" userId) noBody jsonResponse

-- -- | Using either an invite id or hash\/data pair from an email invite
-- --   link, add a user to a team.
-- --
-- --   /Permissions/: Must be authenticated.
-- mmAddUserToTeamFromInvite :: Text -> Text -> InviteId -> Session -> IO TeamMember
-- mmAddUserToTeamFromInvite hash data_ inviteId =
--   inPost (printf "/teams/members/invite?%s" (mkQueryString [ Just ("hash", T.unpack hash) , Just ("data", T.unpack data_) , Just ("invite_id", T.unpack (idString inviteId)) ])) noBody jsonResponse

-- -- | Get a team stats on the system.
-- --
-- --   /Permissions/: Must be authenticated and have the @view_team@
-- --   permission.
-- mmGetTeamStats :: TeamId -> Session -> IO TeamStats
-- mmGetTeamStats teamId =
--   inGet (printf "/teams/%s/stats" teamId) noBody jsonResponse

-- | Get a list of team members based on a provided array of user ids.
--
--   /Permissions/: Must have @view_team@ permission for the team.
mmGetTeamMembersByIds :: TeamId -> Seq UserId -> Session -> IO (Seq TeamMember)
mmGetTeamMembersByIds teamId body =
  inPost (printf "/teams/%s/members/ids" teamId) (jsonBody body) jsonResponse

-- -- | Partially update a team by providing only the fields you want to
-- --   update. Omitted fields will not be updated. The fields that can be
-- --   updated are defined in the request body, all other provided fields
-- --   will be ignored.
-- --
-- --   /Permissions/: Must have the @manage_team@ permission.
-- mmPatchTeam :: TeamId -> XX23 -> Session -> IO Team
-- mmPatchTeam teamId body =
--   inPut (printf "/teams/%s/patch" teamId) (jsonBody body) jsonResponse

-- | Get a list of team members for a user. Useful for getting the ids of
--   teams the user is on and the roles they have in those teams.
--
--   /Permissions/: Must be logged in as the user or have the
--   @edit_other_users@ permission.
mmGetTeamMembersForUser :: UserParam -> Session -> IO (Seq TeamMember)
mmGetTeamMembersForUser userId =
  inGet (printf "/users/%s/teams/members" userId) noBody jsonResponse

-- | Add user to the team by user_id.
--
--   /Permissions/: Must be authenticated and team be open to add self. For
--   adding another user, authenticated user must have the
--   @add_user_to_team@ permission.
mmAddUserToTeam :: TeamId -> TeamMember -> Session -> IO TeamMember
mmAddUserToTeam teamId body =
  inPost (printf "/teams/%s/members" teamId) (jsonBody body) jsonResponse

-- -- | Get a page team members list based on query string parameters - team
-- --   id, page and per page.
-- --
-- --   /Permissions/: Must be authenticated and have the @view_team@
-- --   permission.
-- mmGetTeamMembers :: TeamId -> Maybe Integer -> Maybe Integer -> Session -> IO (Seq TeamMember)
-- mmGetTeamMembers teamId page perPage =
--   inGet (printf "/teams/%s/members?%s" teamId (mkQueryString [ sequence ("page", fmap show page) , sequence ("per_page", fmap show perPage) ])) noBody jsonResponse

-- -- | Import a team into a existing team. Import users, channels, posts,
-- --   hooks.
-- --
-- --   /Permissions/: Must have @permission_import_team@ permission.
-- mmImportTeamFromOtherApplication :: TeamId -> Session -> IO Text
-- mmImportTeamFromOtherApplication teamId =
--   inPost (printf "/teams/%s/import" teamId) noBody jsonResponse

-- -- | Add a number of users to the team by user_id.
-- --
-- --   /Permissions/: Must be authenticated. Authenticated user must have the
-- --   @add_user_to_team@ permission.
-- mmAddMultipleUsersToTeam :: TeamId -> (Seq TeamMember) -> Session -> IO (Seq TeamMember)
-- mmAddMultipleUsersToTeam teamId body =
--   inPost (printf "/teams/%s/members/batch" teamId) (jsonBody body) jsonResponse

-- -- | Get the unread mention and message counts for a team for the specified
-- --   user.
-- --
-- --   /Permissions/: Must be the user or have @edit_other_users@ permission
-- --   and have @view_team@ permission for the team.
-- mmGetUnreadsForTeam :: UserId -> TeamId -> Session -> IO TeamUnread
-- mmGetUnreadsForTeam userId teamId =
--   inGet (printf "/users/%s/teams/%s/unread" userId teamId) noBody jsonResponse

-- -- | Get the @name@, @display_name@, @description@ and @id@ for a team from
-- --   the invite id.
-- --
-- --
-- --   /Minimum server version/: 4.0
-- --
-- --
-- --   /Permissions/: No authentication required.
-- mmGetInviteInfoForTeam :: InviteId -> Session -> IO XX34
-- mmGetInviteInfoForTeam inviteId =
--   inGet (printf "/teams/invite/%s" inviteId) noBody jsonResponse

-- | Get a team based on provided name string
--
--   /Permissions/: Must be authenticated, team type is open and have the
--   @view_team@ permission.
mmGetTeamByName :: Text -> Session -> IO Team
mmGetTeamByName name =
  inGet (printf "/teams/name/%s" name) noBody jsonResponse

-- -- | Update a team by providing the team object. The fields that can be
-- --   updated are defined in the request body, all other provided fields
-- --   will be ignored.
-- --
-- --   /Permissions/: Must have the @manage_team@ permission.
-- mmUpdateTeam :: TeamId -> XX36 -> Session -> IO Team
-- mmUpdateTeam teamId body =
--   inPut (printf "/teams/%s" teamId) (jsonBody body) jsonResponse

-- | Get a team on the system.
--
--   /Permissions/: Must be authenticated and have the @view_team@
--   permission.
mmGetTeam :: TeamId -> Session -> IO Team
mmGetTeam teamId =
  inGet (printf "/teams/%s" teamId) noBody jsonResponse

-- -- | Delete a team softly and put in archived only.
-- --
-- --   /Permissions/: Must have the @manage_team@ permission.
-- mmDeleteTeam :: TeamId -> Bool -> Session -> IO ()
-- mmDeleteTeam teamId permanent =
--   inDelete (printf "/teams/%s?%s" teamId (mkQueryString [ Just ("permanent", if permanent then "true" else "false") ])) noBody jsonResponse



-- * Users

-- | Get a list of users based on search criteria provided in the request
--   body. Searches are typically done against username, full name,
--   nickname and email unless otherwise configured by the server.
--
--   /Permissions/: Requires an active session and @read_channel@ and\/or
--   @view_team@ permissions for any channels or teams specified in the
--   request body.
mmSearchUsers :: UserSearch -> Session -> IO (Seq User)
mmSearchUsers body =
  inPost "/users/search" (jsonBody body) jsonResponse

-- | Get a list of users based on a provided list of usernames.
--
--   /Permissions/: Requires an active session but no other permissions.
mmGetUsersByUsernames :: (Seq Text) -> Session -> IO (Seq User)
mmGetUsersByUsernames body =
  inPost "/users/usernames" (jsonBody body) jsonResponse

-- -- | Revokes a user session from the provided user id and session id
-- --   strings.
-- --
-- --   /Permissions/: Must be logged in as the user being updated or have the
-- --   @edit_other_users@ permission.
-- mmRevokeUserSession :: UserId -> Text -> Session -> IO ()
-- mmRevokeUserSession userId sessionId =
--   inPost (printf "/users/%s/sessions/revoke" userId) (jsonBody (A.object [ "session_id" A..= sessionId ])) jsonResponse

-- | Update a user's system-level roles. Valid user roles are
--   "system_user", "system_admin" or both of them. Overwrites any
--   previously assigned system-level roles.
--
--   /Permissions/: Must have the @manage_roles@ permission.
mmUpdateUsersRoles :: UserId -> Text -> Session -> IO ()
mmUpdateUsersRoles userId roles =
  inPut (printf "/users/%s/roles" userId) (jsonBody (A.object [ "roles" A..= roles ])) noResponse

-- -- | Send an email with a verification link to a user that has an email
-- --   matching the one in the request body. This endpoint will return
-- --   success even if the email does not match any users on the system.
-- --
-- --   /Permissions/: No permissions required.
-- mmSendVerificationEmail :: Text -> Session -> IO ()
-- mmSendVerificationEmail email =
--   inPost "/users/email/verify/send" (jsonBody (A.object [ "email" A..= email ])) jsonResponse

-- -- | Get a list of sessions by providing the user GUID. Sensitive
-- --   information will be sanitized out.
-- --
-- --   /Permissions/: Must be logged in as the user being updated or have the
-- --   @edit_other_users@ permission.
-- -- mmGetUsersSessions :: UserId -> Session -> IO (Seq Session)
-- -- mmGetUsersSessions userId =
-- --   inGet (printf "/users/%s/sessions" userId) noBody jsonResponse

-- -- | Check if a user has multi-factor authentication active on their
-- --   account by providing a login id. Used to check whether an MFA code
-- --   needs to be provided when logging in.
-- --
-- --   /Permissions/: No permission required.
-- mmCheckMfa :: Text -> Session -> IO Bool
-- mmCheckMfa loginId =
--   inPost "/users/mfa" (jsonBody (A.object [ "login_id" A..= loginId ])) jsonResponse

-- -- | Generate a user access token that can be used to authenticate with the
-- --   Mattermost REST API.
-- --
-- --
-- --   /Minimum server version/: 4.1
-- --
-- --
-- --   /Permissions/: Must have @create_user_access_token@ permission. For
-- --   non-self requests, must also have the @edit_other_users@ permission.
-- mmCreateUserAccessToken :: UserId -> Text -> Session -> IO UserAccessToken
-- mmCreateUserAccessToken userId description =
--   inPost (printf "/users/%s/tokens" userId) (jsonBody (A.object [ "description" A..= description ])) jsonResponse

-- -- | Get a list of user access tokens for a user. Does not include the
-- --   actual authentication tokens. Use query paremeters for paging.
-- --
-- --
-- --   /Minimum server version/: 4.1
-- --
-- --
-- --   /Permissions/: Must have @read_user_access_token@ permission. For non-
-- --   self requests, must also have the @edit_other_users@ permission.
-- mmGetUserAccessTokens :: UserId -> Maybe Integer -> Maybe Integer -> Session -> IO (Seq UserAccessTokenSanitized)
-- mmGetUserAccessTokens userId page perPage =
--   inGet (printf "/users/%s/tokens?%s" userId (mkQueryString [ sequence ("page", fmap show page) , sequence ("per_page", fmap show perPage) ])) noBody jsonResponse

-- -- | Revoke a user access token and delete any sessions using the token.
-- --
-- --
-- --   /Minimum server version/: 4.1
-- --
-- --
-- --   /Permissions/: Must have @revoke_user_access_token@ permission. For
-- --   non-self requests, must also have the @edit_other_users@ permission.
-- mmRevokeUserAccessToken :: Text -> Session -> IO ()
-- mmRevokeUserAccessToken token =
--   inPost "/users/tokens/revoke" (jsonBody (A.object [ "token" A..= token ])) jsonResponse

-- -- | Get a user access token. Does not include the actual authentication
-- --   token.
-- --
-- --
-- --   /Minimum server version/: 4.1
-- --
-- --
-- --   /Permissions/: Must have @read_user_access_token@ permission. For non-
-- --   self requests, must also have the @edit_other_users@ permission.
-- mmGetUserAccessToken :: TokenId -> Session -> IO UserAccessTokenSanitized
-- mmGetUserAccessToken tokenId =
--   inGet (printf "/users/tokens/%s" tokenId) noBody jsonResponse

-- -- | Update user active or inactive status
-- --
-- --   /Permissions/: User can deactivate itself.
-- --
-- --   User with @manage_system@ permission can activate or deactivate a
-- --   user.
-- mmUpdateUserActiveStatus :: UserId -> Bool -> Session -> IO ()
-- mmUpdateUserActiveStatus userId active =
--   inPut (printf "/users/%s/active" userId) (jsonBody (A.object [ "active" A..= active ])) jsonResponse

-- -- | Get a user object by providing a username. Sensitive information will
-- --   be sanitized out.
-- --
-- --   /Permissions/: Requires an active session but no other permissions.
-- mmGetUserByUsername :: Text -> Session -> IO User
-- mmGetUserByUsername username =
--   inGet (printf "/users/username/%s" username) noBody jsonResponse

-- | Get a list of users based on a provided list of user ids.
--
--   /Permissions/: Requires an active session but no other permissions.
mmGetUsersByIds :: Seq UserId -> Session -> IO (Seq User)
mmGetUsersByIds body =
  inPost "/users/ids" (jsonBody body) jsonResponse

-- -- | Attach a mobile device id to the currently logged in session. This
-- --   will enable push notiofications for a user, if configured by the
-- --   server.
-- --
-- --   /Permissions/: Must be authenticated.
-- mmAttachMobileDevice :: Text -> Session -> IO ()
-- mmAttachMobileDevice deviceId =
--   inPut "/users/sessions/device" (jsonBody (A.object [ "device_id" A..= deviceId ])) jsonResponse

-- -- | Send an email containing a link for resetting the user's password. The
-- --   link will contain a one-use, timed recovery code tied to the user's
-- --   account. Only works for non-SSO users.
-- --
-- --   /Permissions/: No permissions required.
-- mmSendPasswordResetEmail :: Text -> Session -> IO ()
-- mmSendPasswordResetEmail email =
--   inPost "/users/password/reset/send" (jsonBody (A.object [ "email" A..= email ])) jsonResponse

-- -- | Get a user object by providing a user email. Sensitive information
-- --   will be sanitized out.
-- --
-- --   /Permissions/: Requires an active session but no other permissions.
-- mmGetUserByEmail :: Text -> Session -> IO User
-- mmGetUserByEmail email =
--   inGet (printf "/users/email/%s" email) noBody jsonResponse

-- -- | Switch a user's login method from using email to OAuth2\/SAML\/LDAP or
-- --   back to email. When switching to OAuth2\/SAML, account switching is
-- --   not complete until the user follows the returned link and completes
-- --   any steps on the OAuth2\/SAML service provider.
-- --
-- --
-- --   To switch from email to OAuth2\/SAML, specify @current_service@,
-- --   @new_service@, @email@ and @password@.
-- --
-- --
-- --   To switch from OAuth2\/SAML to email, specify @current_service@,
-- --   @new_service@, @email@ and @new_password@.
-- --
-- --
-- --   To switch from email to LDAP\/AD, specify @current_service@,
-- --   @new_service@, @email@, @password@, @ldap_ip@ and @new_password@ (this
-- --   is the user's LDAP password).
-- --
-- --
-- --   To switch from LDAP\/AD to email, specify @current_service@,
-- --   @new_service@, @ldap_ip@, @password@ (this is the user's LDAP
-- --   password), @email@  and @new_password@.
-- --
-- --
-- --   Additionally, specify @mfa_code@ when trying to switch an account on
-- --   LDAP\/AD or email that has MFA activated.
-- --
-- --
-- --   /Permissions/: No current authentication required except when
-- --   switching from OAuth2\/SAML to email.
-- mmSwitchLoginMethod :: XX19 -> Session -> IO Text
-- mmSwitchLoginMethod body =
--   inPost "/users/login/switch" (jsonBody body) jsonResponse

-- -- | Set a user's profile image based on user_id string parameter.
-- --
-- --   /Permissions/: Must be logged in as the user being updated or have the
-- --   @edit_other_users@ permission.
-- mmSetUsersProfileImage :: UserId -> Session -> IO ()
-- mmSetUsersProfileImage userId =
--   inPost (printf "/users/%s/image" userId) noBody jsonResponse

-- -- | Get a user's profile image based on user_id string parameter.
-- --
-- --   /Permissions/: Must be logged in.
-- mmGetUsersProfileImage :: UserId -> Session -> IO ()
-- mmGetUsersProfileImage userId =
--   inGet (printf "/users/%s/image" userId) noBody jsonResponse

-- -- | Activates multi-factor authentication for the user if @activate@ is
-- --   true and a valid @code@ is provided. If activate is false, then @code@
-- --   is not required and multi-factor authentication is disabled for the
-- --   user.
-- --
-- --   /Permissions/: Must be logged in as the user being updated or have the
-- --   @edit_other_users@ permission.
-- mmUpdateUsersMfa :: UserId -> XX22 -> Session -> IO ()
-- mmUpdateUsersMfa userId body =
--   inPut (printf "/users/%s/mfa" userId) (jsonBody body) jsonResponse

-- -- | Verify the email used by a user to sign-up their account with.
-- --
-- --   /Permissions/: No permissions required.
-- mmVerifyUserEmail :: Text -> Session -> IO ()
-- mmVerifyUserEmail token =
--   inPost "/users/email/verify" (jsonBody (A.object [ "token" A..= token ])) jsonResponse

-- -- | Update the password for a user using a one-use, timed recovery code
-- --   tied to the user's account. Only works for non-SSO users.
-- --
-- --   /Permissions/: No permissions required.
-- mmResetPassword :: XX24 -> Session -> IO ()
-- mmResetPassword body =
--   inPost "/users/password/reset" (jsonBody body) jsonResponse

-- -- | Get a list of audit by providing the user GUID.
-- --
-- --   /Permissions/: Must be logged in as the user or have the
-- --   @edit_other_users@ permission.
-- mmGetUsersAudits :: UserId -> Session -> IO (Seq Audit)
-- mmGetUsersAudits userId =
--   inGet (printf "/users/%s/audits" userId) noBody jsonResponse

-- -- | Update a user's password. New password must meet password policy set
-- --   by server configuration.
-- --
-- --   /Permissions/: Must be logged in as the user the password is being
-- --   changed for or have @manage_system@ permission.
-- mmUpdateUsersPassword :: UserId -> XX29 -> Session -> IO ()
-- mmUpdateUsersPassword userId body =
--   inPut (printf "/users/%s/password" userId) (jsonBody body) jsonResponse

-- -- | Update a user by providing the user object. The fields that can be
-- --   updated are defined in the request body, all other provided fields
-- --   will be ignored.
-- --
-- --   /Permissions/: Must be logged in as the user being updated or have the
-- --   @edit_other_users@ permission.
-- mmUpdateUser :: UserId -> XX30 -> Session -> IO User
-- mmUpdateUser userId body =
--   inPut (printf "/users/%s" userId) (jsonBody body) jsonResponse

-- | Get a user a object. Sensitive information will be sanitized out.
--
--   /Permissions/: Requires an active session but no other permissions.
mmGetUser :: UserParam -> Session -> IO User
mmGetUser userId =
  inGet (printf "/users/%s" userId) noBody jsonResponse

-- | Deactivates the user by archiving its user object.
--
--   /Permissions/: Must be logged in as the user being deactivated or have
--   the @edit_other_users@ permission.
mmDeactivateUserAccount :: UserParam -> Session -> IO ()
mmDeactivateUserAccount userId =
  inDelete (printf "/users/%s" userId) noBody jsonResponse

-- | Create a new user on the system.
--
--   /Permissions/: No permission required but user creation can be
--   controlled by server configuration.
mmCreateUser :: UsersCreate -> Session -> IO User
mmCreateUser body =
  inPost "/users" (jsonBody body) jsonResponse
  -- UsersCreate was XX31

data UserQuery = UserQuery
  { userQueryPage         :: Maybe Int
  , userQueryPerPage      :: Maybe Int
  , userQueryInTeam       :: Maybe TeamId
  , userQueryNotInTeam    :: Maybe TeamId
  , userQueryInChannel    :: Maybe ChannelId
  , userQueryNotInChannel :: Maybe ChannelId
  , userQueryWithoutTeam  :: Maybe Bool
  , userQuerySort         :: Maybe UserQuerySort
  }

defaultUserQuery :: UserQuery
defaultUserQuery = UserQuery
  { userQueryPage         = Nothing
  , userQueryPerPage      = Nothing
  , userQueryInTeam       = Nothing
  , userQueryNotInTeam    = Nothing
  , userQueryInChannel    = Nothing
  , userQueryNotInChannel = Nothing
  , userQueryWithoutTeam  = Nothing
  , userQuerySort         = Nothing
  }

data UserQuerySort
  = UserQuerySortByLastActivity
  | UserQuerySortByCreation

userQueryToQueryString :: UserQuery -> String
userQueryToQueryString UserQuery { .. } =
  mkQueryString [ sequence ("page", fmap show userQueryPage)
                , sequence ("per_page", fmap show userQueryPerPage)
                , sequence ("in_team", fmap (T.unpack . idString) userQueryInTeam)
                , sequence ("not_in_team", fmap (T.unpack . idString) userQueryNotInTeam)
                , sequence ("in_channel", fmap (T.unpack . idString) userQueryInChannel)
                , sequence ("not_in_channel", fmap (T.unpack . idString) userQueryNotInChannel)
                , sequence ( "without_team"
                           , case userQueryWithoutTeam of
                             Nothing -> Nothing
                             Just True -> Just "true"
                             Just False -> Just "false"
                           )
                , sequence ( "sort"
                           , case userQuerySort of
                               Nothing -> Nothing
                               Just UserQuerySortByLastActivity -> Just "last_activity_at"
                               Just UserQuerySortByCreation -> Just "create_at"
                           )
                ]

-- | Get a page of a list of users. Based on query string parameters,
--   select users from a team, channel, or select users not in a specific
--   channel.
--
--
--   Since server version 4.0, some basic sorting is available using the
--   @sort@ query parameter. Sorting is currently only supported when
--   selecting users on a team.
--
--   /Permissions/: Requires an active session and (if specified)
--   membership to the channel or team being selected from.
mmGetUsers
  :: UserQuery
  -> Session
  -> IO (Seq User)
mmGetUsers userQuery =
  inGet (printf "/users?%s" (userQueryToQueryString userQuery)) noBody jsonResponse

-- -- | Generates an multi-factor authentication secret for a user and returns
-- --   it as a string and as base64 encoded QR code image.
-- --
-- --   /Permissions/: Must be logged in as the user or have the
-- --   @edit_other_users@ permission.
-- mmGenerateMfaSecret :: UserId -> Session -> IO XX37
-- mmGenerateMfaSecret userId =
--   inPost (printf "/users/%s/mfa/generate" userId) noBody jsonResponse

-- -- | Partially update a user by providing only the fields you want to
-- --   update. Omitted fields will not be updated. The fields that can be
-- --   updated are defined in the request body, all other provided fields
-- --   will be ignored.
-- --
-- --   /Permissions/: Must be logged in as the user being updated or have the
-- --   @edit_other_users@ permission.
-- mmPatchUser :: UserId -> XX38 -> Session -> IO User
-- mmPatchUser userId body =
--   inPut (printf "/users/%s/patch" userId) (jsonBody body) jsonResponse

-- | Get a list of users for the purpose of autocompleting based on the
--   provided search term. Specify a combination of @team_id@ and
--   @channel_id@ to filter results further.
--
--   /Permissions/: Requires an active session and @view_team@ and
--   @read_channel@ on any teams or channels used to filter the results
--   further.
mmAutocompleteUsers :: Maybe TeamId
                    -> Maybe ChannelId
                    -> Text -> Session -> IO UserAutocomplete
mmAutocompleteUsers mTeamId mChannelId name =
    let queryString = mkQueryString args
        args = [ (("team_id",) . T.unpack . idString) <$> mTeamId
               , (("channel_id",) . T.unpack . idString) <$> mChannelId
               , Just ("name", T.unpack name)
               ]
    in inGet (printf "/users/autocomplete?%s" queryString) noBody jsonResponse

-- | Get a list of channels for the purpose of autocompleting based on
--   the provided search term.
mmAutocompleteChannels :: TeamId -> Text -> Session -> IO (Seq Channel)
mmAutocompleteChannels teamId name =
    let queryString = mkQueryString args
        args = [ Just ("team_id", T.unpack $ idString teamId)
               , Just ("name", T.unpack name)
               ]
    in inGet (printf "/teams/%s/channels/autocomplete?%s" teamId queryString)
             noBody jsonResponse

-- * Webhooks

-- -- | Update an outgoing webhook given the hook id.
-- --
-- --   /Permissions/: @manage_webhooks@ for system or @manage_webhooks@ for
-- --   the specific team or @manage_webhooks@ for the channel.
-- mmUpdateAnOutgoingWebhook :: HookId -> XX12 -> Session -> IO OutgoingWebhook
-- mmUpdateAnOutgoingWebhook hookId body =
--   inPut (printf "/hooks/outgoing/%s" hookId) (jsonBody body) jsonResponse

-- -- | Get an outgoing webhook given the hook id.
-- --
-- --   /Permissions/: @manage_webhooks@ for system or @manage_webhooks@ for
-- --   the specific team or @manage_webhooks@ for the channel.
-- mmGetAnOutgoingWebhook :: HookId -> Session -> IO OutgoingWebhook
-- mmGetAnOutgoingWebhook hookId =
--   inGet (printf "/hooks/outgoing/%s" hookId) noBody jsonResponse

-- -- | Delete an outgoing webhook given the hook id.
-- --
-- --   /Permissions/: @manage_webhooks@ for system or @manage_webhooks@ for
-- --   the specific team or @manage_webhooks@ for the channel.
-- mmDeleteAnOutgoingWebhook :: HookId -> Session -> IO ()
-- mmDeleteAnOutgoingWebhook hookId =
--   inDelete (printf "/hooks/outgoing/%s" hookId) noBody jsonResponse

-- -- | Regenerate the token for the outgoing webhook.
-- --
-- --   /Permissions/: @manage_webhooks@ for system or @manage_webhooks@ for
-- --   the specific team or @manage_webhooks@ for the channel.
-- mmRegenerateTokenForOutgoingWebhook :: HookId -> Session -> IO ()
-- mmRegenerateTokenForOutgoingWebhook hookId =
--   inPost (printf "/hooks/outgoing/%s/regen_token" hookId) noBody jsonResponse

-- -- | Create an incoming webhook for a channel.
-- --
-- --   /Permissions/: @manage_webhooks@ for the channel the webhook is in.
-- mmCreateAnIncomingWebhook :: XX25 -> Session -> IO IncomingWebhook
-- mmCreateAnIncomingWebhook body =
--   inPost "/hooks/incoming" (jsonBody body) jsonResponse

-- -- | Get a page of a list of incoming webhooks. Optionally filter for a
-- --   specific team using query parameters.
-- --
-- --   /Permissions/: @manage_webhooks@ for the system or @manage_webhooks@
-- --   for the specific team.
-- mmListIncomingWebhooks :: Maybe Integer -> Maybe Integer -> TeamId -> Session -> IO (Seq IncomingWebhook)
-- mmListIncomingWebhooks page perPage teamId =
--   inGet (printf "/hooks/incoming?%s" (mkQueryString [ sequence ("page", fmap show page) , sequence ("per_page", fmap show perPage) , Just ("team_id", T.unpack (idString teamId)) ])) noBody jsonResponse

-- -- | Create an outgoing webhook for a team.
-- --
-- --   /Permissions/: @manage_webhooks@ for the team the webhook is in.
-- mmCreateAnOutgoingWebhook :: XX33 -> Session -> IO OutgoingWebhook
-- mmCreateAnOutgoingWebhook body =
--   inPost "/hooks/outgoing" (jsonBody body) jsonResponse

-- -- | Get a page of a list of outgoing webhooks. Optionally filter for a
-- --   specific team or channel using query parameters.
-- --
-- --   /Permissions/: @manage_webhooks@ for the system or @manage_webhooks@
-- --   for the specific team\/channel.
-- mmListOutgoingWebhooks :: Maybe Integer -> Maybe Integer -> TeamId -> ChannelId -> Session -> IO (Seq OutgoingWebhook)
-- mmListOutgoingWebhooks page perPage teamId channelId =
--   inGet (printf "/hooks/outgoing?%s" (mkQueryString [ sequence ("page", fmap show page) , sequence ("per_page", fmap show perPage) , Just ("team_id", T.unpack (idString teamId)) , Just ("channel_id", T.unpack (idString channelId)) ])) noBody jsonResponse

-- -- | Update an incoming webhook given the hook id.
-- --
-- --   /Permissions/: @manage_webhooks@ for system or @manage_webhooks@ for
-- --   the specific team or @manage_webhooks@ for the channel.
-- mmUpdateAnIncomingWebhook :: HookId -> XX35 -> Session -> IO IncomingWebhook
-- mmUpdateAnIncomingWebhook hookId body =
--   inPut (printf "/hooks/incoming/%s" hookId) (jsonBody body) jsonResponse

-- -- | Get an incoming webhook given the hook id.
-- --
-- --   /Permissions/: @manage_webhooks@ for system or @manage_webhooks@ for
-- --   the specific team or @manage_webhooks@ for the channel.
-- mmGetAnIncomingWebhook :: HookId -> Session -> IO IncomingWebhook
-- mmGetAnIncomingWebhook hookId =
--   inGet (printf "/hooks/incoming/%s" hookId) noBody jsonResponse

-- data AppError = AppError
--   { appErrorStatusCode :: Integer
--   , appErrorMessage :: Text
--   , appErrorId :: Text
--   , appErrorRequestId :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON AppError where
--   parseJSON = A.withObject "appError" $ \v -> do
--     appErrorStatusCode <- v A..: "status_code"
--     appErrorMessage <- v A..: "message"
--     appErrorId <- v A..: "id"
--     appErrorRequestId <- v A..: "request_id"
--     return AppError { .. }

-- instance A.ToJSON AppError where
--   toJSON AppError { .. } = A.object
--     [ "status_code" A..= appErrorStatusCode
--     , "message" A..= appErrorMessage
--     , "id" A..= appErrorId
--     , "request_id" A..= appErrorRequestId
--     ]

-- ** User Statuses

mmGetUserStatus :: UserParam -> Session -> IO T.Text
mmGetUserStatus userId =
  inGet (printf "/users/%s/status" userId) noBody jsonResponse

getUserStatusesByIds :: Seq UserId -> Session -> IO (HM.HashMap UserId T.Text)
getUserStatusesByIds body =
  inPost (printf "/users/%s/status/ids") (jsonBody body) jsonResponse

-- updateUserStatus :: UserParam -> Session -> IO ()
-- updateUserStatus =
--   inPut

-- --

-- data Article = Article
--   { articlePublishedTime :: Text
--   , articleTags :: (Seq Text)
--   , articleSection :: Text
--   , articleAuthors :: (Seq XX4)
--   , articleExpirationTime :: Text
--   , articleModifiedTime :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON Article where
--   parseJSON = A.withObject "article" $ \v -> do
--     articlePublishedTime <- v A..: "published_time"
--     articleTags <- v A..: "tags"
--     articleSection <- v A..: "section"
--     articleAuthors <- v A..: "authors"
--     articleExpirationTime <- v A..: "expiration_time"
--     articleModifiedTime <- v A..: "modified_time"
--     return Article { .. }

-- instance A.ToJSON Article where
--   toJSON Article { .. } = A.object
--     [ "published_time" A..= articlePublishedTime
--     , "tags" A..= articleTags
--     , "section" A..= articleSection
--     , "authors" A..= articleAuthors
--     , "expiration_time" A..= articleExpirationTime
--     , "modified_time" A..= articleModifiedTime
--     ]

-- --

-- data Audit = Audit
--   { auditUserId :: Text
--   , auditAction :: Text
--   , auditExtraInfo :: Text
--   , auditIpAddress :: Text
--   , auditCreateAt :: UnknownType
--   , auditSessionId :: Text
--   , auditId :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON Audit where
--   parseJSON = A.withObject "audit" $ \v -> do
--     auditUserId <- v A..: "user_id"
--     auditAction <- v A..: "action"
--     auditExtraInfo <- v A..: "extra_info"
--     auditIpAddress <- v A..: "ip_address"
--     auditCreateAt <- v A..: "create_at"
--     auditSessionId <- v A..: "session_id"
--     auditId <- v A..: "id"
--     return Audit { .. }

-- instance A.ToJSON Audit where
--   toJSON Audit { .. } = A.object
--     [ "user_id" A..= auditUserId
--     , "action" A..= auditAction
--     , "extra_info" A..= auditExtraInfo
--     , "ip_address" A..= auditIpAddress
--     , "create_at" A..= auditCreateAt
--     , "session_id" A..= auditSessionId
--     , "id" A..= auditId
--     ]

-- --

-- data Book = Book
--   { bookReleaseDate :: Text
--   , bookTags :: (Seq Text)
--   , bookIsbn :: Text
--   , bookAuthors :: (Seq XX2)
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON Book where
--   parseJSON = A.withObject "book" $ \v -> do
--     bookReleaseDate <- v A..: "release_date"
--     bookTags <- v A..: "tags"
--     bookIsbn <- v A..: "isbn"
--     bookAuthors <- v A..: "authors"
--     return Book { .. }

-- instance A.ToJSON Book where
--   toJSON Book { .. } = A.object
--     [ "release_date" A..= bookReleaseDate
--     , "tags" A..= bookTags
--     , "isbn" A..= bookIsbn
--     , "authors" A..= bookAuthors
--     ]

-- --

-- --

-- data ClusterInfo = ClusterInfo
--   { clusterInfoLastPing :: Integer
--   , clusterInfoVersion :: Text
--     -- ^ The server version the node is on
--   , clusterInfoInternodeUrl :: Text
--     -- ^ The URL used to communicate with those node from other nodes
--   , clusterInfoConfigHash :: Text
--     -- ^ The hash of the configuartion file the node is using
--   , clusterInfoHostname :: Text
--     -- ^ The hostname for this node
--   , clusterInfoIsAlive :: UnknownType
--     -- ^ Whether or not the node is alive and well
--   , clusterInfoId :: Text
--     -- ^ The unique ID for the node
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON ClusterInfo where
--   parseJSON = A.withObject "clusterInfo" $ \v -> do
--     clusterInfoLastPing <- v A..: "last_ping"
--     clusterInfoVersion <- v A..: "version"
--     clusterInfoInternodeUrl <- v A..: "internode_url"
--     clusterInfoConfigHash <- v A..: "config_hash"
--     clusterInfoHostname <- v A..: "hostname"
--     clusterInfoIsAlive <- v A..: "is_alive"
--     clusterInfoId <- v A..: "id"
--     return ClusterInfo { .. }

-- instance A.ToJSON ClusterInfo where
--   toJSON ClusterInfo { .. } = A.object
--     [ "last_ping" A..= clusterInfoLastPing
--     , "version" A..= clusterInfoVersion
--     , "internode_url" A..= clusterInfoInternodeUrl
--     , "config_hash" A..= clusterInfoConfigHash
--     , "hostname" A..= clusterInfoHostname
--     , "is_alive" A..= clusterInfoIsAlive
--     , "id" A..= clusterInfoId
--     ]

-- --

-- data ClusterSettings = ClusterSettings
--   { clusterSettingsInternodeurls :: (Seq Text)
--   , clusterSettingsEnable :: UnknownType
--   , clusterSettingsInternodelistenaddress :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON ClusterSettings where
--   parseJSON = A.withObject "clusterSettings" $ \v -> do
--     clusterSettingsInternodeurls <- v A..: "InterNodeUrls"
--     clusterSettingsEnable <- v A..: "Enable"
--     clusterSettingsInternodelistenaddress <- v A..: "InterNodeListenAddress"
--     return ClusterSettings { .. }

-- instance A.ToJSON ClusterSettings where
--   toJSON ClusterSettings { .. } = A.object
--     [ "InterNodeUrls" A..= clusterSettingsInternodeurls
--     , "Enable" A..= clusterSettingsEnable
--     , "InterNodeListenAddress" A..= clusterSettingsInternodelistenaddress
--     ]

-- --

-- data Compliance = Compliance
--   { complianceStatus :: Text
--   , complianceCount :: UnknownType
--   , complianceUserId :: Text
--   , complianceId :: Text
--   , complianceCreateAt :: UnknownType
--   , complianceEndAt :: UnknownType
--   , complianceKeywords :: Text
--   , complianceStartAt :: UnknownType
--   , complianceType :: Text
--   , complianceEmails :: Text
--   , complianceDesc :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON Compliance where
--   parseJSON = A.withObject "compliance" $ \v -> do
--     complianceStatus <- v A..: "status"
--     complianceCount <- v A..: "count"
--     complianceUserId <- v A..: "user_id"
--     complianceId <- v A..: "id"
--     complianceCreateAt <- v A..: "create_at"
--     complianceEndAt <- v A..: "end_at"
--     complianceKeywords <- v A..: "keywords"
--     complianceStartAt <- v A..: "start_at"
--     complianceType <- v A..: "type"
--     complianceEmails <- v A..: "emails"
--     complianceDesc <- v A..: "desc"
--     return Compliance { .. }

-- instance A.ToJSON Compliance where
--   toJSON Compliance { .. } = A.object
--     [ "status" A..= complianceStatus
--     , "count" A..= complianceCount
--     , "user_id" A..= complianceUserId
--     , "id" A..= complianceId
--     , "create_at" A..= complianceCreateAt
--     , "end_at" A..= complianceEndAt
--     , "keywords" A..= complianceKeywords
--     , "start_at" A..= complianceStartAt
--     , "type" A..= complianceType
--     , "emails" A..= complianceEmails
--     , "desc" A..= complianceDesc
--     ]

-- --

-- --

-- --

-- data Emoji = Emoji
--   { emojiCreatorId :: Text
--   , emojiName :: Text
--     -- ^ The name of the emoji
--   , emojiDeleteAt :: UnknownType
--     -- ^ The time at which the emoji was deleted.
--   , emojiUpdateAt :: UnknownType
--     -- ^ The time at which the emoji was updated.
--   , emojiCreateAt :: UnknownType
--     -- ^ The time at which the emoji was made
--   , emojiId :: Text
--     -- ^ The ID of the emoji
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON Emoji where
--   parseJSON = A.withObject "emoji" $ \v -> do
--     emojiCreatorId <- v A..: "creator_id"
--     emojiName <- v A..: "name"
--     emojiDeleteAt <- v A..: "delete_at"
--     emojiUpdateAt <- v A..: "update_at"
--     emojiCreateAt <- v A..: "create_at"
--     emojiId <- v A..: "id"
--     return Emoji { .. }

-- instance A.ToJSON Emoji where
--   toJSON Emoji { .. } = A.object
--     [ "creator_id" A..= emojiCreatorId
--     , "name" A..= emojiName
--     , "delete_at" A..= emojiDeleteAt
--     , "update_at" A..= emojiUpdateAt
--     , "create_at" A..= emojiCreateAt
--     , "id" A..= emojiId
--     ]

-- --

-- data FileSettings = FileSettings
--   { fileSettingsInitialfont :: Text
--   , fileSettingsThumbnailwidth :: UnknownType
--   , fileSettingsAmazons3accesskeyid :: Text
--   , fileSettingsAmazons3region :: Text
--   , fileSettingsPreviewwidth :: UnknownType
--   , fileSettingsAmazons3endpoint :: Text
--   , fileSettingsDirectory :: Text
--   , fileSettingsThumbnailheight :: UnknownType
--   , fileSettingsAmazons3bucket :: Text
--   , fileSettingsAmazons3secretaccesskey :: Text
--   , fileSettingsAmazons3ssl :: UnknownType
--   , fileSettingsPreviewheight :: UnknownType
--   , fileSettingsEnablepubliclink :: UnknownType
--   , fileSettingsMaxfilesize :: UnknownType
--   , fileSettingsProfilewidth :: UnknownType
--   , fileSettingsProfileheight :: UnknownType
--   , fileSettingsPubliclinksalt :: Text
--   , fileSettingsDrivername :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON FileSettings where
--   parseJSON = A.withObject "fileSettings" $ \v -> do
--     fileSettingsInitialfont <- v A..: "InitialFont"
--     fileSettingsThumbnailwidth <- v A..: "ThumbnailWidth"
--     fileSettingsAmazons3accesskeyid <- v A..: "AmazonS3AccessKeyId"
--     fileSettingsAmazons3region <- v A..: "AmazonS3Region"
--     fileSettingsPreviewwidth <- v A..: "PreviewWidth"
--     fileSettingsAmazons3endpoint <- v A..: "AmazonS3Endpoint"
--     fileSettingsDirectory <- v A..: "Directory"
--     fileSettingsThumbnailheight <- v A..: "ThumbnailHeight"
--     fileSettingsAmazons3bucket <- v A..: "AmazonS3Bucket"
--     fileSettingsAmazons3secretaccesskey <- v A..: "AmazonS3SecretAccessKey"
--     fileSettingsAmazons3ssl <- v A..: "AmazonS3SSL"
--     fileSettingsPreviewheight <- v A..: "PreviewHeight"
--     fileSettingsEnablepubliclink <- v A..: "EnablePublicLink"
--     fileSettingsMaxfilesize <- v A..: "MaxFileSize"
--     fileSettingsProfilewidth <- v A..: "ProfileWidth"
--     fileSettingsProfileheight <- v A..: "ProfileHeight"
--     fileSettingsPubliclinksalt <- v A..: "PublicLinkSalt"
--     fileSettingsDrivername <- v A..: "DriverName"
--     return FileSettings { .. }

-- instance A.ToJSON FileSettings where
--   toJSON FileSettings { .. } = A.object
--     [ "InitialFont" A..= fileSettingsInitialfont
--     , "ThumbnailWidth" A..= fileSettingsThumbnailwidth
--     , "AmazonS3AccessKeyId" A..= fileSettingsAmazons3accesskeyid
--     , "AmazonS3Region" A..= fileSettingsAmazons3region
--     , "PreviewWidth" A..= fileSettingsPreviewwidth
--     , "AmazonS3Endpoint" A..= fileSettingsAmazons3endpoint
--     , "Directory" A..= fileSettingsDirectory
--     , "ThumbnailHeight" A..= fileSettingsThumbnailheight
--     , "AmazonS3Bucket" A..= fileSettingsAmazons3bucket
--     , "AmazonS3SecretAccessKey" A..= fileSettingsAmazons3secretaccesskey
--     , "AmazonS3SSL" A..= fileSettingsAmazons3ssl
--     , "PreviewHeight" A..= fileSettingsPreviewheight
--     , "EnablePublicLink" A..= fileSettingsEnablepubliclink
--     , "MaxFileSize" A..= fileSettingsMaxfilesize
--     , "ProfileWidth" A..= fileSettingsProfilewidth
--     , "ProfileHeight" A..= fileSettingsProfileheight
--     , "PublicLinkSalt" A..= fileSettingsPubliclinksalt
--     , "DriverName" A..= fileSettingsDrivername
--     ]

-- --

-- data GitLabSettings = GitLabSettings
--   { gitLabSettingsSecret :: Text
--   , gitLabSettingsEnable :: UnknownType
--   , gitLabSettingsScope :: Text
--   , gitLabSettingsUserapiendpoint :: Text
--   , gitLabSettingsTokenendpoint :: Text
--   , gitLabSettingsAuthendpoint :: Text
--   , gitLabSettingsId :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON GitLabSettings where
--   parseJSON = A.withObject "gitLabSettings" $ \v -> do
--     gitLabSettingsSecret <- v A..: "Secret"
--     gitLabSettingsEnable <- v A..: "Enable"
--     gitLabSettingsScope <- v A..: "Scope"
--     gitLabSettingsUserapiendpoint <- v A..: "UserApiEndpoint"
--     gitLabSettingsTokenendpoint <- v A..: "TokenEndpoint"
--     gitLabSettingsAuthendpoint <- v A..: "AuthEndpoint"
--     gitLabSettingsId <- v A..: "Id"
--     return GitLabSettings { .. }

-- instance A.ToJSON GitLabSettings where
--   toJSON GitLabSettings { .. } = A.object
--     [ "Secret" A..= gitLabSettingsSecret
--     , "Enable" A..= gitLabSettingsEnable
--     , "Scope" A..= gitLabSettingsScope
--     , "UserApiEndpoint" A..= gitLabSettingsUserapiendpoint
--     , "TokenEndpoint" A..= gitLabSettingsTokenendpoint
--     , "AuthEndpoint" A..= gitLabSettingsAuthendpoint
--     , "Id" A..= gitLabSettingsId
--     ]

-- --

-- data GoogleSettings = GoogleSettings
--   { googleSettingsSecret :: Text
--   , googleSettingsEnable :: UnknownType
--   , googleSettingsScope :: Text
--   , googleSettingsUserapiendpoint :: Text
--   , googleSettingsTokenendpoint :: Text
--   , googleSettingsAuthendpoint :: Text
--   , googleSettingsId :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON GoogleSettings where
--   parseJSON = A.withObject "googleSettings" $ \v -> do
--     googleSettingsSecret <- v A..: "Secret"
--     googleSettingsEnable <- v A..: "Enable"
--     googleSettingsScope <- v A..: "Scope"
--     googleSettingsUserapiendpoint <- v A..: "UserApiEndpoint"
--     googleSettingsTokenendpoint <- v A..: "TokenEndpoint"
--     googleSettingsAuthendpoint <- v A..: "AuthEndpoint"
--     googleSettingsId <- v A..: "Id"
--     return GoogleSettings { .. }

-- instance A.ToJSON GoogleSettings where
--   toJSON GoogleSettings { .. } = A.object
--     [ "Secret" A..= googleSettingsSecret
--     , "Enable" A..= googleSettingsEnable
--     , "Scope" A..= googleSettingsScope
--     , "UserApiEndpoint" A..= googleSettingsUserapiendpoint
--     , "TokenEndpoint" A..= googleSettingsTokenendpoint
--     , "AuthEndpoint" A..= googleSettingsAuthendpoint
--     , "Id" A..= googleSettingsId
--     ]

-- --

-- data IncomingWebhook = IncomingWebhook
--   { incomingWebhookChannelId :: Text
--   , incomingWebhookDisplayName :: Text
--     -- ^ The display name for this incoming webhook
--   , incomingWebhookDescription :: Text
--     -- ^ The description for this incoming webhook
--   , incomingWebhookDeleteAt :: UnknownType
--   , incomingWebhookUpdateAt :: UnknownType
--   , incomingWebhookCreateAt :: UnknownType
--   , incomingWebhookId :: Text
--     -- ^ The unique identifier for this incoming webhook
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON IncomingWebhook where
--   parseJSON = A.withObject "incomingWebhook" $ \v -> do
--     incomingWebhookChannelId <- v A..: "channel_id"
--     incomingWebhookDisplayName <- v A..: "display_name"
--     incomingWebhookDescription <- v A..: "description"
--     incomingWebhookDeleteAt <- v A..: "delete_at"
--     incomingWebhookUpdateAt <- v A..: "update_at"
--     incomingWebhookCreateAt <- v A..: "create_at"
--     incomingWebhookId <- v A..: "id"
--     return IncomingWebhook { .. }

-- instance A.ToJSON IncomingWebhook where
--   toJSON IncomingWebhook { .. } = A.object
--     [ "channel_id" A..= incomingWebhookChannelId
--     , "display_name" A..= incomingWebhookDisplayName
--     , "description" A..= incomingWebhookDescription
--     , "delete_at" A..= incomingWebhookDeleteAt
--     , "update_at" A..= incomingWebhookUpdateAt
--     , "create_at" A..= incomingWebhookCreateAt
--     , "id" A..= incomingWebhookId
--     ]

-- --

-- data Job = Job
--   { jobStatus :: Text
--   , jobStartAt :: UnknownType
--     -- ^ The time at which the job was started
--   , jobType :: Text
--     -- ^ The type of job
--   , jobCreateAt :: UnknownType
--     -- ^ The time at which the job was created
--   , jobId :: Text
--     -- ^ The unique id of the job
--   , jobProgress :: UnknownType
--     -- ^ The progress (as a percentage) of the job
--   , jobData :: UnknownType
--     -- ^ A freeform data field containing additional information about the job
--   , jobLastActivityAt :: UnknownType
--     -- ^ The last time at which the job had activity
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON Job where
--   parseJSON = A.withObject "job" $ \v -> do
--     jobStatus <- v A..: "status"
--     jobStartAt <- v A..: "start_at"
--     jobType <- v A..: "type"
--     jobCreateAt <- v A..: "create_at"
--     jobId <- v A..: "id"
--     jobProgress <- v A..: "progress"
--     jobData <- v A..: "data"
--     jobLastActivityAt <- v A..: "last_activity_at"
--     return Job { .. }

-- instance A.ToJSON Job where
--   toJSON Job { .. } = A.object
--     [ "status" A..= jobStatus
--     , "start_at" A..= jobStartAt
--     , "type" A..= jobType
--     , "create_at" A..= jobCreateAt
--     , "id" A..= jobId
--     , "progress" A..= jobProgress
--     , "data" A..= jobData
--     , "last_activity_at" A..= jobLastActivityAt
--     ]

-- --

-- data LdapSettings = LdapSettings
--   { ldapSettingsLastnameattribute :: Text
--   , ldapSettingsIdattribute :: Text
--   , ldapSettingsSyncintervalminutes :: UnknownType
--   , ldapSettingsLoginfieldname :: Text
--   , ldapSettingsLdapserver :: Text
--   , ldapSettingsLdapport :: UnknownType
--   , ldapSettingsUsernameattribute :: Text
--   , ldapSettingsMaxpagesize :: UnknownType
--   , ldapSettingsEnable :: UnknownType
--   , ldapSettingsUserfilter :: Text
--   , ldapSettingsBindpassword :: Text
--   , ldapSettingsSkipcertificateverification :: UnknownType
--   , ldapSettingsQuerytimeout :: UnknownType
--   , ldapSettingsBasedn :: Text
--   , ldapSettingsPositionattribute :: Text
--   , ldapSettingsEmailattribute :: Text
--   , ldapSettingsConnectionsecurity :: Text
--   , ldapSettingsBindusername :: Text
--   , ldapSettingsFirstnameattribute :: Text
--   , ldapSettingsNicknameattribute :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON LdapSettings where
--   parseJSON = A.withObject "ldapSettings" $ \v -> do
--     ldapSettingsLastnameattribute <- v A..: "LastNameAttribute"
--     ldapSettingsIdattribute <- v A..: "IdAttribute"
--     ldapSettingsSyncintervalminutes <- v A..: "SyncIntervalMinutes"
--     ldapSettingsLoginfieldname <- v A..: "LoginFieldName"
--     ldapSettingsLdapserver <- v A..: "LdapServer"
--     ldapSettingsLdapport <- v A..: "LdapPort"
--     ldapSettingsUsernameattribute <- v A..: "UsernameAttribute"
--     ldapSettingsMaxpagesize <- v A..: "MaxPageSize"
--     ldapSettingsEnable <- v A..: "Enable"
--     ldapSettingsUserfilter <- v A..: "UserFilter"
--     ldapSettingsBindpassword <- v A..: "BindPassword"
--     ldapSettingsSkipcertificateverification <- v A..: "SkipCertificateVerification"
--     ldapSettingsQuerytimeout <- v A..: "QueryTimeout"
--     ldapSettingsBasedn <- v A..: "BaseDN"
--     ldapSettingsPositionattribute <- v A..: "PositionAttribute"
--     ldapSettingsEmailattribute <- v A..: "EmailAttribute"
--     ldapSettingsConnectionsecurity <- v A..: "ConnectionSecurity"
--     ldapSettingsBindusername <- v A..: "BindUsername"
--     ldapSettingsFirstnameattribute <- v A..: "FirstNameAttribute"
--     ldapSettingsNicknameattribute <- v A..: "NicknameAttribute"
--     return LdapSettings { .. }

-- instance A.ToJSON LdapSettings where
--   toJSON LdapSettings { .. } = A.object
--     [ "LastNameAttribute" A..= ldapSettingsLastnameattribute
--     , "IdAttribute" A..= ldapSettingsIdattribute
--     , "SyncIntervalMinutes" A..= ldapSettingsSyncintervalminutes
--     , "LoginFieldName" A..= ldapSettingsLoginfieldname
--     , "LdapServer" A..= ldapSettingsLdapserver
--     , "LdapPort" A..= ldapSettingsLdapport
--     , "UsernameAttribute" A..= ldapSettingsUsernameattribute
--     , "MaxPageSize" A..= ldapSettingsMaxpagesize
--     , "Enable" A..= ldapSettingsEnable
--     , "UserFilter" A..= ldapSettingsUserfilter
--     , "BindPassword" A..= ldapSettingsBindpassword
--     , "SkipCertificateVerification" A..= ldapSettingsSkipcertificateverification
--     , "QueryTimeout" A..= ldapSettingsQuerytimeout
--     , "BaseDN" A..= ldapSettingsBasedn
--     , "PositionAttribute" A..= ldapSettingsPositionattribute
--     , "EmailAttribute" A..= ldapSettingsEmailattribute
--     , "ConnectionSecurity" A..= ldapSettingsConnectionsecurity
--     , "BindUsername" A..= ldapSettingsBindusername
--     , "FirstNameAttribute" A..= ldapSettingsFirstnameattribute
--     , "NicknameAttribute" A..= ldapSettingsNicknameattribute
--     ]

-- --

-- data LocalizationSettings = LocalizationSettings
--   { localizationSettingsDefaultclientlocale :: Text
--   , localizationSettingsAvailablelocales :: Text
--   , localizationSettingsDefaultserverlocale :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON LocalizationSettings where
--   parseJSON = A.withObject "localizationSettings" $ \v -> do
--     localizationSettingsDefaultclientlocale <- v A..: "DefaultClientLocale"
--     localizationSettingsAvailablelocales <- v A..: "AvailableLocales"
--     localizationSettingsDefaultserverlocale <- v A..: "DefaultServerLocale"
--     return LocalizationSettings { .. }

-- instance A.ToJSON LocalizationSettings where
--   toJSON LocalizationSettings { .. } = A.object
--     [ "DefaultClientLocale" A..= localizationSettingsDefaultclientlocale
--     , "AvailableLocales" A..= localizationSettingsAvailablelocales
--     , "DefaultServerLocale" A..= localizationSettingsDefaultserverlocale
--     ]

-- --

-- --

-- --

-- data OAuthApp = OAuthApp
--   { oAuthAppDescription :: Text
--   , oAuthAppIconUrl :: Text
--     -- ^ A URL to an icon to display with the application
--   , oAuthAppUpdateAt :: UnknownType
--     -- ^ The last time of update for the application
--   , oAuthAppCreateAt :: UnknownType
--     -- ^ The time of registration for the application
--   , oAuthAppIsTrusted :: UnknownType
--     -- ^ Set this to `true` to skip asking users for permission
--   , oAuthAppClientSecret :: Text
--     -- ^ The client secret of the application
--   , oAuthAppCallbackUrls :: (Seq Text)
--     -- ^ A list of callback URLs for the appliation
--   , oAuthAppHomepage :: Text
--     -- ^ A link to the website of the application
--   , oAuthAppId :: Text
--     -- ^ The client id of the application
--   , oAuthAppName :: Text
--     -- ^ The name of the client application
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON OAuthApp where
--   parseJSON = A.withObject "oAuthApp" $ \v -> do
--     oAuthAppDescription <- v A..: "description"
--     oAuthAppIconUrl <- v A..: "icon_url"
--     oAuthAppUpdateAt <- v A..: "update_at"
--     oAuthAppCreateAt <- v A..: "create_at"
--     oAuthAppIsTrusted <- v A..: "is_trusted"
--     oAuthAppClientSecret <- v A..: "client_secret"
--     oAuthAppCallbackUrls <- v A..: "callback_urls"
--     oAuthAppHomepage <- v A..: "homepage"
--     oAuthAppId <- v A..: "id"
--     oAuthAppName <- v A..: "name"
--     return OAuthApp { .. }

-- instance A.ToJSON OAuthApp where
--   toJSON OAuthApp { .. } = A.object
--     [ "description" A..= oAuthAppDescription
--     , "icon_url" A..= oAuthAppIconUrl
--     , "update_at" A..= oAuthAppUpdateAt
--     , "create_at" A..= oAuthAppCreateAt
--     , "is_trusted" A..= oAuthAppIsTrusted
--     , "client_secret" A..= oAuthAppClientSecret
--     , "callback_urls" A..= oAuthAppCallbackUrls
--     , "homepage" A..= oAuthAppHomepage
--     , "id" A..= oAuthAppId
--     , "name" A..= oAuthAppName
--     ]

-- --

-- data Office365Settings = Office365Settings
--   { office365SettingsSecret :: Text
--   , office365SettingsEnable :: UnknownType
--   , office365SettingsScope :: Text
--   , office365SettingsUserapiendpoint :: Text
--   , office365SettingsTokenendpoint :: Text
--   , office365SettingsAuthendpoint :: Text
--   , office365SettingsId :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON Office365Settings where
--   parseJSON = A.withObject "office365Settings" $ \v -> do
--     office365SettingsSecret <- v A..: "Secret"
--     office365SettingsEnable <- v A..: "Enable"
--     office365SettingsScope <- v A..: "Scope"
--     office365SettingsUserapiendpoint <- v A..: "UserApiEndpoint"
--     office365SettingsTokenendpoint <- v A..: "TokenEndpoint"
--     office365SettingsAuthendpoint <- v A..: "AuthEndpoint"
--     office365SettingsId <- v A..: "Id"
--     return Office365Settings { .. }

-- instance A.ToJSON Office365Settings where
--   toJSON Office365Settings { .. } = A.object
--     [ "Secret" A..= office365SettingsSecret
--     , "Enable" A..= office365SettingsEnable
--     , "Scope" A..= office365SettingsScope
--     , "UserApiEndpoint" A..= office365SettingsUserapiendpoint
--     , "TokenEndpoint" A..= office365SettingsTokenendpoint
--     , "AuthEndpoint" A..= office365SettingsAuthendpoint
--     , "Id" A..= office365SettingsId
--     ]

-- --

-- data OpenGraph = OpenGraph
--   { openGraphProfile :: UnknownObject
--   , openGraphSiteName :: Text
--   , openGraphDescription :: Text
--   , openGraphVideos :: (Seq XX1)
--   , openGraphTitle :: Text
--   , openGraphUrl :: Text
--   , openGraphLocalesAlternate :: (Seq Text)
--   , openGraphLocale :: Text
--   , openGraphBook :: Book
--     -- ^ Book object used in OpenGraph metadata of a webpage, if type is book
--   , openGraphImages :: (Seq XX3)
--   , openGraphArticle :: Article
--     -- ^ Article object used in OpenGraph metadata of a webpage, if type is article
--   , openGraphAudios :: (Seq XX5)
--   , openGraphType :: Text
--   , openGraphDeterminer :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON OpenGraph where
--   parseJSON = A.withObject "openGraph" $ \v -> do
--     openGraphProfile <- v A..: "profile"
--     openGraphSiteName <- v A..: "site_name"
--     openGraphDescription <- v A..: "description"
--     openGraphVideos <- v A..: "videos"
--     openGraphTitle <- v A..: "title"
--     openGraphUrl <- v A..: "url"
--     openGraphLocalesAlternate <- v A..: "locales_alternate"
--     openGraphLocale <- v A..: "locale"
--     openGraphBook <- v A..: "book"
--     openGraphImages <- v A..: "images"
--     openGraphArticle <- v A..: "article"
--     openGraphAudios <- v A..: "audios"
--     openGraphType <- v A..: "type"
--     openGraphDeterminer <- v A..: "determiner"
--     return OpenGraph { .. }

-- instance A.ToJSON OpenGraph where
--   toJSON OpenGraph { .. } = A.object
--     [ "profile" A..= openGraphProfile
--     , "site_name" A..= openGraphSiteName
--     , "description" A..= openGraphDescription
--     , "videos" A..= openGraphVideos
--     , "title" A..= openGraphTitle
--     , "url" A..= openGraphUrl
--     , "locales_alternate" A..= openGraphLocalesAlternate
--     , "locale" A..= openGraphLocale
--     , "book" A..= openGraphBook
--     , "images" A..= openGraphImages
--     , "article" A..= openGraphArticle
--     , "audios" A..= openGraphAudios
--     , "type" A..= openGraphType
--     , "determiner" A..= openGraphDeterminer
--     ]

-- --

-- data OutgoingWebhook = OutgoingWebhook
--   { outgoingWebhookTriggerWhen :: Integer
--   , outgoingWebhookDisplayName :: Text
--     -- ^ The display name for this outgoing webhook
--   , outgoingWebhookDescription :: Text
--     -- ^ The description for this outgoing webhook
--   , outgoingWebhookDeleteAt :: UnknownType
--   , outgoingWebhookUpdateAt :: UnknownType
--   , outgoingWebhookCreateAt :: UnknownType
--   , outgoingWebhookChannelId :: Text
--     -- ^ The ID of a public channel that the webhook watchs
--   , outgoingWebhookCreatorId :: Text
--     -- ^ The Id of the user who created the webhook
--   , outgoingWebhookContentType :: Text
--     -- ^ The format to POST the data in, either `application/json` or `application/x-www-form-urlencoded`
--   , outgoingWebhookTriggerWords :: (Seq Text)
--     -- ^ List of words for the webhook to trigger on
--   , outgoingWebhookTeamId :: Text
--     -- ^ The ID of the team that the webhook watchs
--   , outgoingWebhookCallbackUrls :: (Seq Text)
--     -- ^ The URLs to POST the payloads to when the webhook is triggered
--   , outgoingWebhookId :: Text
--     -- ^ The unique identifier for this outgoing webhook
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON OutgoingWebhook where
--   parseJSON = A.withObject "outgoingWebhook" $ \v -> do
--     outgoingWebhookTriggerWhen <- v A..: "trigger_when"
--     outgoingWebhookDisplayName <- v A..: "display_name"
--     outgoingWebhookDescription <- v A..: "description"
--     outgoingWebhookDeleteAt <- v A..: "delete_at"
--     outgoingWebhookUpdateAt <- v A..: "update_at"
--     outgoingWebhookCreateAt <- v A..: "create_at"
--     outgoingWebhookChannelId <- v A..: "channel_id"
--     outgoingWebhookCreatorId <- v A..: "creator_id"
--     outgoingWebhookContentType <- v A..: "content_type"
--     outgoingWebhookTriggerWords <- v A..: "trigger_words"
--     outgoingWebhookTeamId <- v A..: "team_id"
--     outgoingWebhookCallbackUrls <- v A..: "callback_urls"
--     outgoingWebhookId <- v A..: "id"
--     return OutgoingWebhook { .. }

-- instance A.ToJSON OutgoingWebhook where
--   toJSON OutgoingWebhook { .. } = A.object
--     [ "trigger_when" A..= outgoingWebhookTriggerWhen
--     , "display_name" A..= outgoingWebhookDisplayName
--     , "description" A..= outgoingWebhookDescription
--     , "delete_at" A..= outgoingWebhookDeleteAt
--     , "update_at" A..= outgoingWebhookUpdateAt
--     , "create_at" A..= outgoingWebhookCreateAt
--     , "channel_id" A..= outgoingWebhookChannelId
--     , "creator_id" A..= outgoingWebhookCreatorId
--     , "content_type" A..= outgoingWebhookContentType
--     , "trigger_words" A..= outgoingWebhookTriggerWords
--     , "team_id" A..= outgoingWebhookTeamId
--     , "callback_urls" A..= outgoingWebhookCallbackUrls
--     , "id" A..= outgoingWebhookId
--     ]

-- --

-- --

-- data SamlCertificateStatus = SamlCertificateStatus
--   { samlCertificateStatusIdpCertificateFile :: Bool
--   , samlCertificateStatusPrivateKeyFile :: UnknownType
--     -- ^ Status is good when `true`
--   , samlCertificateStatusPublicCertificateFile :: UnknownType
--     -- ^ Status is good when `true`
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON SamlCertificateStatus where
--   parseJSON = A.withObject "samlCertificateStatus" $ \v -> do
--     samlCertificateStatusIdpCertificateFile <- v A..: "idp_certificate_file"
--     samlCertificateStatusPrivateKeyFile <- v A..: "private_key_file"
--     samlCertificateStatusPublicCertificateFile <- v A..: "public_certificate_file"
--     return SamlCertificateStatus { .. }

-- instance A.ToJSON SamlCertificateStatus where
--   toJSON SamlCertificateStatus { .. } = A.object
--     [ "idp_certificate_file" A..= samlCertificateStatusIdpCertificateFile
--     , "private_key_file" A..= samlCertificateStatusPrivateKeyFile
--     , "public_certificate_file" A..= samlCertificateStatusPublicCertificateFile
--     ]

-- --

-- data SamlSettings = SamlSettings
--   { samlSettingsLoginbuttontext :: Text
--   , samlSettingsLastnameattribute :: Text
--   , samlSettingsEncrypt :: UnknownType
--   , samlSettingsIdpurl :: Text
--   , samlSettingsVerify :: UnknownType
--   , samlSettingsAssertionconsumerserviceurl :: Text
--   , samlSettingsUsernameattribute :: Text
--   , samlSettingsLocaleattribute :: Text
--   , samlSettingsFirstnameattribute :: Text
--   , samlSettingsEnable :: UnknownType
--   , samlSettingsNicknameattribute :: Text
--   , samlSettingsPositionattribute :: Text
--   , samlSettingsIdpdescriptorurl :: Text
--   , samlSettingsPrivatekeyfile :: Text
--   , samlSettingsIdpcertificatefile :: Text
--   , samlSettingsEmailattribute :: Text
--   , samlSettingsPubliccertificatefile :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON SamlSettings where
--   parseJSON = A.withObject "samlSettings" $ \v -> do
--     samlSettingsLoginbuttontext <- v A..: "LoginButtonText"
--     samlSettingsLastnameattribute <- v A..: "LastNameAttribute"
--     samlSettingsEncrypt <- v A..: "Encrypt"
--     samlSettingsIdpurl <- v A..: "IdpUrl"
--     samlSettingsVerify <- v A..: "Verify"
--     samlSettingsAssertionconsumerserviceurl <- v A..: "AssertionConsumerServiceURL"
--     samlSettingsUsernameattribute <- v A..: "UsernameAttribute"
--     samlSettingsLocaleattribute <- v A..: "LocaleAttribute"
--     samlSettingsFirstnameattribute <- v A..: "FirstNameAttribute"
--     samlSettingsEnable <- v A..: "Enable"
--     samlSettingsNicknameattribute <- v A..: "NicknameAttribute"
--     samlSettingsPositionattribute <- v A..: "PositionAttribute"
--     samlSettingsIdpdescriptorurl <- v A..: "IdpDescriptorUrl"
--     samlSettingsPrivatekeyfile <- v A..: "PrivateKeyFile"
--     samlSettingsIdpcertificatefile <- v A..: "IdpCertificateFile"
--     samlSettingsEmailattribute <- v A..: "EmailAttribute"
--     samlSettingsPubliccertificatefile <- v A..: "PublicCertificateFile"
--     return SamlSettings { .. }

-- instance A.ToJSON SamlSettings where
--   toJSON SamlSettings { .. } = A.object
--     [ "LoginButtonText" A..= samlSettingsLoginbuttontext
--     , "LastNameAttribute" A..= samlSettingsLastnameattribute
--     , "Encrypt" A..= samlSettingsEncrypt
--     , "IdpUrl" A..= samlSettingsIdpurl
--     , "Verify" A..= samlSettingsVerify
--     , "AssertionConsumerServiceURL" A..= samlSettingsAssertionconsumerserviceurl
--     , "UsernameAttribute" A..= samlSettingsUsernameattribute
--     , "LocaleAttribute" A..= samlSettingsLocaleattribute
--     , "FirstNameAttribute" A..= samlSettingsFirstnameattribute
--     , "Enable" A..= samlSettingsEnable
--     , "NicknameAttribute" A..= samlSettingsNicknameattribute
--     , "PositionAttribute" A..= samlSettingsPositionattribute
--     , "IdpDescriptorUrl" A..= samlSettingsIdpdescriptorurl
--     , "PrivateKeyFile" A..= samlSettingsPrivatekeyfile
--     , "IdpCertificateFile" A..= samlSettingsIdpcertificatefile
--     , "EmailAttribute" A..= samlSettingsEmailattribute
--     , "PublicCertificateFile" A..= samlSettingsPubliccertificatefile
--     ]

-- --

-- data ServiceSettings = ServiceSettings
--   { serviceSettingsEnableposticonoverride :: Bool
--   , serviceSettingsSegmentdeveloperkey :: Text
--   , serviceSettingsEnablepostusernameoverride :: UnknownType
--   , serviceSettingsForward80to443 :: UnknownType
--   , serviceSettingsEnableincomingwebhooks :: UnknownType
--   , serviceSettingsSessionlengthmobileindays :: UnknownType
--   , serviceSettingsUseletsencrypt :: UnknownType
--   , serviceSettingsRestrictcustomemojicreation :: Text
--   , serviceSettingsReadtimeout :: UnknownType
--   , serviceSettingsEnableoutgoingwebhooks :: UnknownType
--   , serviceSettingsTlscertfile :: Text
--   , serviceSettingsEnableonlyadminintegrations :: UnknownType
--   , serviceSettingsEnableinsecureoutgoingconnections :: UnknownType
--   , serviceSettingsEnableoauthserviceprovider :: UnknownType
--   , serviceSettingsEnablecustomemoji :: UnknownType
--   , serviceSettingsEnabletesting :: UnknownType
--   , serviceSettingsSessioncacheinminutes :: UnknownType
--   , serviceSettingsSessionlengthwebindays :: UnknownType
--   , serviceSettingsWebservermode :: Text
--   , serviceSettingsEnablesecurityfixalert :: UnknownType
--   , serviceSettingsEnablemultifactorauthentication :: UnknownType
--   , serviceSettingsEnabledeveloper :: UnknownType
--   , serviceSettingsSiteurl :: Text
--   , serviceSettingsTlskeyfile :: Text
--   , serviceSettingsListenaddress :: Text
--   , serviceSettingsGoogledeveloperkey :: Text
--   , serviceSettingsEnforcemultifactorauthentication :: UnknownType
--   , serviceSettingsLetsencryptcertificatecachefile :: Text
--   , serviceSettingsWebsocketport :: UnknownType
--   , serviceSettingsWebsocketsecureport :: UnknownType
--   , serviceSettingsAllowcorsfrom :: Text
--   , serviceSettingsSessionlengthssoindays :: UnknownType
--   , serviceSettingsEnablecommands :: UnknownType
--   , serviceSettingsConnectionsecurity :: Text
--   , serviceSettingsWritetimeout :: UnknownType
--   , serviceSettingsMaximumloginattempts :: UnknownType
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON ServiceSettings where
--   parseJSON = A.withObject "serviceSettings" $ \v -> do
--     serviceSettingsEnableposticonoverride <- v A..: "EnablePostIconOverride"
--     serviceSettingsSegmentdeveloperkey <- v A..: "SegmentDeveloperKey"
--     serviceSettingsEnablepostusernameoverride <- v A..: "EnablePostUsernameOverride"
--     serviceSettingsForward80to443 <- v A..: "Forward80To443"
--     serviceSettingsEnableincomingwebhooks <- v A..: "EnableIncomingWebhooks"
--     serviceSettingsSessionlengthmobileindays <- v A..: "SessionLengthMobileInDays"
--     serviceSettingsUseletsencrypt <- v A..: "UseLetsEncrypt"
--     serviceSettingsRestrictcustomemojicreation <- v A..: "RestrictCustomEmojiCreation"
--     serviceSettingsReadtimeout <- v A..: "ReadTimeout"
--     serviceSettingsEnableoutgoingwebhooks <- v A..: "EnableOutgoingWebhooks"
--     serviceSettingsTlscertfile <- v A..: "TLSCertFile"
--     serviceSettingsEnableonlyadminintegrations <- v A..: "EnableOnlyAdminIntegrations"
--     serviceSettingsEnableinsecureoutgoingconnections <- v A..: "EnableInsecureOutgoingConnections"
--     serviceSettingsEnableoauthserviceprovider <- v A..: "EnableOAuthServiceProvider"
--     serviceSettingsEnablecustomemoji <- v A..: "EnableCustomEmoji"
--     serviceSettingsEnabletesting <- v A..: "EnableTesting"
--     serviceSettingsSessioncacheinminutes <- v A..: "SessionCacheInMinutes"
--     serviceSettingsSessionlengthwebindays <- v A..: "SessionLengthWebInDays"
--     serviceSettingsWebservermode <- v A..: "WebserverMode"
--     serviceSettingsEnablesecurityfixalert <- v A..: "EnableSecurityFixAlert"
--     serviceSettingsEnablemultifactorauthentication <- v A..: "EnableMultifactorAuthentication"
--     serviceSettingsEnabledeveloper <- v A..: "EnableDeveloper"
--     serviceSettingsSiteurl <- v A..: "SiteURL"
--     serviceSettingsTlskeyfile <- v A..: "TLSKeyFile"
--     serviceSettingsListenaddress <- v A..: "ListenAddress"
--     serviceSettingsGoogledeveloperkey <- v A..: "GoogleDeveloperKey"
--     serviceSettingsEnforcemultifactorauthentication <- v A..: "EnforceMultifactorAuthentication"
--     serviceSettingsLetsencryptcertificatecachefile <- v A..: "LetsEncryptCertificateCacheFile"
--     serviceSettingsWebsocketport <- v A..: "WebsocketPort"
--     serviceSettingsWebsocketsecureport <- v A..: "WebsocketSecurePort"
--     serviceSettingsAllowcorsfrom <- v A..: "AllowCorsFrom"
--     serviceSettingsSessionlengthssoindays <- v A..: "SessionLengthSSOInDays"
--     serviceSettingsEnablecommands <- v A..: "EnableCommands"
--     serviceSettingsConnectionsecurity <- v A..: "ConnectionSecurity"
--     serviceSettingsWritetimeout <- v A..: "WriteTimeout"
--     serviceSettingsMaximumloginattempts <- v A..: "MaximumLoginAttempts"
--     return ServiceSettings { .. }

-- instance A.ToJSON ServiceSettings where
--   toJSON ServiceSettings { .. } = A.object
--     [ "EnablePostIconOverride" A..= serviceSettingsEnableposticonoverride
--     , "SegmentDeveloperKey" A..= serviceSettingsSegmentdeveloperkey
--     , "EnablePostUsernameOverride" A..= serviceSettingsEnablepostusernameoverride
--     , "Forward80To443" A..= serviceSettingsForward80to443
--     , "EnableIncomingWebhooks" A..= serviceSettingsEnableincomingwebhooks
--     , "SessionLengthMobileInDays" A..= serviceSettingsSessionlengthmobileindays
--     , "UseLetsEncrypt" A..= serviceSettingsUseletsencrypt
--     , "RestrictCustomEmojiCreation" A..= serviceSettingsRestrictcustomemojicreation
--     , "ReadTimeout" A..= serviceSettingsReadtimeout
--     , "EnableOutgoingWebhooks" A..= serviceSettingsEnableoutgoingwebhooks
--     , "TLSCertFile" A..= serviceSettingsTlscertfile
--     , "EnableOnlyAdminIntegrations" A..= serviceSettingsEnableonlyadminintegrations
--     , "EnableInsecureOutgoingConnections" A..= serviceSettingsEnableinsecureoutgoingconnections
--     , "EnableOAuthServiceProvider" A..= serviceSettingsEnableoauthserviceprovider
--     , "EnableCustomEmoji" A..= serviceSettingsEnablecustomemoji
--     , "EnableTesting" A..= serviceSettingsEnabletesting
--     , "SessionCacheInMinutes" A..= serviceSettingsSessioncacheinminutes
--     , "SessionLengthWebInDays" A..= serviceSettingsSessionlengthwebindays
--     , "WebserverMode" A..= serviceSettingsWebservermode
--     , "EnableSecurityFixAlert" A..= serviceSettingsEnablesecurityfixalert
--     , "EnableMultifactorAuthentication" A..= serviceSettingsEnablemultifactorauthentication
--     , "EnableDeveloper" A..= serviceSettingsEnabledeveloper
--     , "SiteURL" A..= serviceSettingsSiteurl
--     , "TLSKeyFile" A..= serviceSettingsTlskeyfile
--     , "ListenAddress" A..= serviceSettingsListenaddress
--     , "GoogleDeveloperKey" A..= serviceSettingsGoogledeveloperkey
--     , "EnforceMultifactorAuthentication" A..= serviceSettingsEnforcemultifactorauthentication
--     , "LetsEncryptCertificateCacheFile" A..= serviceSettingsLetsencryptcertificatecachefile
--     , "WebsocketPort" A..= serviceSettingsWebsocketport
--     , "WebsocketSecurePort" A..= serviceSettingsWebsocketsecureport
--     , "AllowCorsFrom" A..= serviceSettingsAllowcorsfrom
--     , "SessionLengthSSOInDays" A..= serviceSettingsSessionlengthssoindays
--     , "EnableCommands" A..= serviceSettingsEnablecommands
--     , "ConnectionSecurity" A..= serviceSettingsConnectionsecurity
--     , "WriteTimeout" A..= serviceSettingsWritetimeout
--     , "MaximumLoginAttempts" A..= serviceSettingsMaximumloginattempts
--     ]

-- --

-- data SlackAttachment = SlackAttachment
--   { slackAttachmentTitlelink :: Text
--   , slackAttachmentFooter :: Text
--   , slackAttachmentFields :: (Seq SlackAttachmentField)
--   , slackAttachmentImageurl :: Text
--   , slackAttachmentAuthorname :: Text
--   , slackAttachmentThumburl :: Text
--   , slackAttachmentTitle :: Text
--   , slackAttachmentFallback :: Text
--   , slackAttachmentColor :: Text
--   , slackAttachmentText :: Text
--   , slackAttachmentAuthorlink :: Text
--   , slackAttachmentAuthoricon :: Text
--   , slackAttachmentTimestamp :: Text
--     -- ^ The timestamp of the slack attahment, either type of string or integer
--   , slackAttachmentPretext :: Text
--   , slackAttachmentId :: Text
--   , slackAttachmentFootericon :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON SlackAttachment where
--   parseJSON = A.withObject "slackAttachment" $ \v -> do
--     slackAttachmentTitlelink <- v A..: "TitleLink"
--     slackAttachmentFooter <- v A..: "Footer"
--     slackAttachmentFields <- v A..: "Fields"
--     slackAttachmentImageurl <- v A..: "ImageURL"
--     slackAttachmentAuthorname <- v A..: "AuthorName"
--     slackAttachmentThumburl <- v A..: "ThumbURL"
--     slackAttachmentTitle <- v A..: "Title"
--     slackAttachmentFallback <- v A..: "Fallback"
--     slackAttachmentColor <- v A..: "Color"
--     slackAttachmentText <- v A..: "Text"
--     slackAttachmentAuthorlink <- v A..: "AuthorLink"
--     slackAttachmentAuthoricon <- v A..: "AuthorIcon"
--     slackAttachmentTimestamp <- v A..: "Timestamp"
--     slackAttachmentPretext <- v A..: "Pretext"
--     slackAttachmentId <- v A..: "Id"
--     slackAttachmentFootericon <- v A..: "FooterIcon"
--     return SlackAttachment { .. }

-- instance A.ToJSON SlackAttachment where
--   toJSON SlackAttachment { .. } = A.object
--     [ "TitleLink" A..= slackAttachmentTitlelink
--     , "Footer" A..= slackAttachmentFooter
--     , "Fields" A..= slackAttachmentFields
--     , "ImageURL" A..= slackAttachmentImageurl
--     , "AuthorName" A..= slackAttachmentAuthorname
--     , "ThumbURL" A..= slackAttachmentThumburl
--     , "Title" A..= slackAttachmentTitle
--     , "Fallback" A..= slackAttachmentFallback
--     , "Color" A..= slackAttachmentColor
--     , "Text" A..= slackAttachmentText
--     , "AuthorLink" A..= slackAttachmentAuthorlink
--     , "AuthorIcon" A..= slackAttachmentAuthoricon
--     , "Timestamp" A..= slackAttachmentTimestamp
--     , "Pretext" A..= slackAttachmentPretext
--     , "Id" A..= slackAttachmentId
--     , "FooterIcon" A..= slackAttachmentFootericon
--     ]

-- --

-- data SlackAttachmentField = SlackAttachmentField
--   { slackAttachmentFieldShort :: Bool
--   , slackAttachmentFieldValue :: Text
--     -- ^ The value of the attachment, set as string but capable with golang interface
--   , slackAttachmentFieldTitle :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON SlackAttachmentField where
--   parseJSON = A.withObject "slackAttachmentField" $ \v -> do
--     slackAttachmentFieldShort <- v A..: "Short"
--     slackAttachmentFieldValue <- v A..: "Value"
--     slackAttachmentFieldTitle <- v A..: "Title"
--     return SlackAttachmentField { .. }

-- instance A.ToJSON SlackAttachmentField where
--   toJSON SlackAttachmentField { .. } = A.object
--     [ "Short" A..= slackAttachmentFieldShort
--     , "Value" A..= slackAttachmentFieldValue
--     , "Title" A..= slackAttachmentFieldTitle
--     ]

-- --

-- data Status = Status
--   { statusStatus :: Text
--   , statusActiveChannel :: Text
--   , statusManual :: UnknownType
--   , statusLastActivityAt :: UnknownType
--   , statusUserId :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON Status where
--   parseJSON = A.withObject "status" $ \v -> do
--     statusStatus <- v A..: "status"
--     statusActiveChannel <- v A..: "active_channel"
--     statusManual <- v A..: "manual"
--     statusLastActivityAt <- v A..: "last_activity_at"
--     statusUserId <- v A..: "user_id"
--     return Status { .. }

-- instance A.ToJSON Status where
--   toJSON Status { .. } = A.object
--     [ "status" A..= statusStatus
--     , "active_channel" A..= statusActiveChannel
--     , "manual" A..= statusManual
--     , "last_activity_at" A..= statusLastActivityAt
--     , "user_id" A..= statusUserId
--     ]

-- --

-- newtype StatusOK = StatusOK
--   { statusOKStatus :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON StatusOK where
--   parseJSON = A.withObject "statusOK" $ \v -> do
--     statusOKStatus <- v A..: "status"
--     return StatusOK { .. }

-- instance A.ToJSON StatusOK where
--   toJSON StatusOK { .. } = A.object
--     [ "status" A..= statusOKStatus
--     ]

-- --

-- --

-- newtype TeamExists = TeamExists
--   { teamExistsExists :: Bool
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON TeamExists where
--   parseJSON = A.withObject "teamExists" $ \v -> do
--     teamExistsExists <- v A..: "exists"
--     return TeamExists { .. }

-- instance A.ToJSON TeamExists where
--   toJSON TeamExists { .. } = A.object
--     [ "exists" A..= teamExistsExists
--     ]

-- --

-- newtype TeamMap = TeamMap
--   { teamMapTeamId :: Team
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON TeamMap where
--   parseJSON = A.withObject "teamMap" $ \v -> do
--     teamMapTeamId <- v A..: "team_id"
--     return TeamMap { .. }

-- instance A.ToJSON TeamMap where
--   toJSON TeamMap { .. } = A.object
--     [ "team_id" A..= teamMapTeamId
--     ]

-- --

-- --

-- data TeamStats = TeamStats
--   { teamStatsTeamId :: Text
--   , teamStatsActiveMemberCount :: UnknownType
--   , teamStatsTotalMemberCount :: UnknownType
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON TeamStats where
--   parseJSON = A.withObject "teamStats" $ \v -> do
--     teamStatsTeamId <- v A..: "team_id"
--     teamStatsActiveMemberCount <- v A..: "active_member_count"
--     teamStatsTotalMemberCount <- v A..: "total_member_count"
--     return TeamStats { .. }

-- instance A.ToJSON TeamStats where
--   toJSON TeamStats { .. } = A.object
--     [ "team_id" A..= teamStatsTeamId
--     , "active_member_count" A..= teamStatsActiveMemberCount
--     , "total_member_count" A..= teamStatsTotalMemberCount
--     ]

-- --

-- data TeamUnread = TeamUnread
--   { teamUnreadTeamId :: Text
--   , teamUnreadMsgCount :: UnknownType
--   , teamUnreadMentionCount :: UnknownType
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON TeamUnread where
--   parseJSON = A.withObject "teamUnread" $ \v -> do
--     teamUnreadTeamId <- v A..: "team_id"
--     teamUnreadMsgCount <- v A..: "msg_count"
--     teamUnreadMentionCount <- v A..: "mention_count"
--     return TeamUnread { .. }

-- instance A.ToJSON TeamUnread where
--   toJSON TeamUnread { .. } = A.object
--     [ "team_id" A..= teamUnreadTeamId
--     , "msg_count" A..= teamUnreadMsgCount
--     , "mention_count" A..= teamUnreadMentionCount
--     ]

-- --

-- data UserAccessToken = UserAccessToken
--   { userAccessTokenToken :: Text
--   , userAccessTokenUserId :: Text
--     -- ^ The user the token authenticates for
--   , userAccessTokenId :: Text
--     -- ^ Unique identifier for the token
--   , userAccessTokenDescription :: Text
--     -- ^ A description of the token usage
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON UserAccessToken where
--   parseJSON = A.withObject "userAccessToken" $ \v -> do
--     userAccessTokenToken <- v A..: "token"
--     userAccessTokenUserId <- v A..: "user_id"
--     userAccessTokenId <- v A..: "id"
--     userAccessTokenDescription <- v A..: "description"
--     return UserAccessToken { .. }

-- instance A.ToJSON UserAccessToken where
--   toJSON UserAccessToken { .. } = A.object
--     [ "token" A..= userAccessTokenToken
--     , "user_id" A..= userAccessTokenUserId
--     , "id" A..= userAccessTokenId
--     , "description" A..= userAccessTokenDescription
--     ]

-- --

-- data UserAccessTokenSanitized = UserAccessTokenSanitized
--   { userAccessTokenSanitizedUserId :: Text
--   , userAccessTokenSanitizedId :: Text
--     -- ^ Unique identifier for the token
--   , userAccessTokenSanitizedDescription :: Text
--     -- ^ A description of the token usage
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON UserAccessTokenSanitized where
--   parseJSON = A.withObject "userAccessTokenSanitized" $ \v -> do
--     userAccessTokenSanitizedUserId <- v A..: "user_id"
--     userAccessTokenSanitizedId <- v A..: "id"
--     userAccessTokenSanitizedDescription <- v A..: "description"
--     return UserAccessTokenSanitized { .. }

-- instance A.ToJSON UserAccessTokenSanitized where
--   toJSON UserAccessTokenSanitized { .. } = A.object
--     [ "user_id" A..= userAccessTokenSanitizedUserId
--     , "id" A..= userAccessTokenSanitizedId
--     , "description" A..= userAccessTokenSanitizedDescription
--     ]

-- --

data UserAutocomplete = UserAutocomplete
  { userAutocompleteUsers :: Seq User
  , userAutocompleteOutOfChannel :: Maybe (Seq User)
    -- ^ A special case list of users returned when autocompleting in a
    -- specific channel. Omitted when empty or not relevant
  } deriving (Read, Show, Eq)

instance A.FromJSON UserAutocomplete where
  parseJSON = A.withObject "userAutocomplete" $ \v -> do
    userAutocompleteUsers <- v A..: "users"
    userAutocompleteOutOfChannel <- v A..:? "out_of_channel"
    return UserAutocomplete { .. }

instance A.ToJSON UserAutocomplete where
  toJSON UserAutocomplete { .. } = A.object
    [ "users" A..= userAutocompleteUsers
    , "out_of_channel" A..= userAutocompleteOutOfChannel
    ]

-- --

-- data UserAutocompleteInChannel = UserAutocompleteInChannel
--   { userAutocompleteInChannelInChannel :: (Seq User)
--   , userAutocompleteInChannelOutOfChannel :: (Seq User)
--     -- ^ A list of user objects not in the channel
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON UserAutocompleteInChannel where
--   parseJSON = A.withObject "userAutocompleteInChannel" $ \v -> do
--     userAutocompleteInChannelInChannel <- v A..: "in_channel"
--     userAutocompleteInChannelOutOfChannel <- v A..: "out_of_channel"
--     return UserAutocompleteInChannel { .. }

-- instance A.ToJSON UserAutocompleteInChannel where
--   toJSON UserAutocompleteInChannel { .. } = A.object
--     [ "in_channel" A..= userAutocompleteInChannelInChannel
--     , "out_of_channel" A..= userAutocompleteInChannelOutOfChannel
--     ]

-- --

-- newtype UserAutocompleteInTeam = UserAutocompleteInTeam
--   { userAutocompleteInTeamInTeam :: (Seq User)
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON UserAutocompleteInTeam where
--   parseJSON = A.withObject "userAutocompleteInTeam" $ \v -> do
--     userAutocompleteInTeamInTeam <- v A..: "in_team"
--     return UserAutocompleteInTeam { .. }

-- instance A.ToJSON UserAutocompleteInTeam where
--   toJSON UserAutocompleteInTeam { .. } = A.object
--     [ "in_team" A..= userAutocompleteInTeamInTeam
--     ]

-- --

-- --

-- data XX1 = XX1
--   { xx1Url :: Text
--   , xx1Width :: UnknownType
--   , xx1SecureUrl :: Text
--   , xx1Type :: Text
--   , xx1Height :: UnknownType
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX1 where
--   parseJSON = A.withObject "xx1" $ \v -> do
--     xx1Url <- v A..: "url"
--     xx1Width <- v A..: "width"
--     xx1SecureUrl <- v A..: "secure_url"
--     xx1Type <- v A..: "type"
--     xx1Height <- v A..: "height"
--     return XX1 { .. }

-- instance A.ToJSON XX1 where
--   toJSON XX1 { .. } = A.object
--     [ "url" A..= xx1Url
--     , "width" A..= xx1Width
--     , "secure_url" A..= xx1SecureUrl
--     , "type" A..= xx1Type
--     , "height" A..= xx1Height
--     ]

-- --

-- data XX10 = XX10
--   { xx10IsOrSearch :: Bool
--   , xx10Terms :: Text
--     -- ^ The search terms as inputed by the user.
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX10 where
--   parseJSON = A.withObject "xx10" $ \v -> do
--     xx10IsOrSearch <- v A..: "is_or_search"
--     xx10Terms <- v A..: "terms"
--     return XX10 { .. }

-- instance A.ToJSON XX10 where
--   toJSON XX10 { .. } = A.object
--     [ "is_or_search" A..= xx10IsOrSearch
--     , "terms" A..= xx10Terms
--     ]

-- --

-- --

-- data XX12 = XX12
--   { xx12HookId :: Text
--   , xx12ChannelId :: Text
--     -- ^ The ID of a public channel or private group that receives the webhook payloads.
--   , xx12DisplayName :: Text
--     -- ^ The display name for this incoming webhook
--   , xx12Description :: Text
--     -- ^ The description for this incoming webhook
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX12 where
--   parseJSON = A.withObject "xx12" $ \v -> do
--     xx12HookId <- v A..: "hook_id"
--     xx12ChannelId <- v A..: "channel_id"
--     xx12DisplayName <- v A..: "display_name"
--     xx12Description <- v A..: "description"
--     return XX12 { .. }

-- instance A.ToJSON XX12 where
--   toJSON XX12 { .. } = A.object
--     [ "hook_id" A..= xx12HookId
--     , "channel_id" A..= xx12ChannelId
--     , "display_name" A..= xx12DisplayName
--     , "description" A..= xx12Description
--     ]

-- --

-- data XX13 = XX13
--   { xx13Name :: Text
--   , xx13IconUrl :: Text
--     -- ^ A URL to an icon to display with the application
--   , xx13CallbackUrls :: (Seq Text)
--     -- ^ A list of callback URLs for the appliation
--   , xx13Homepage :: Text
--     -- ^ A link to the website of the application
--   , xx13IsTrusted :: UnknownType
--     -- ^ Set this to `true` to skip asking users for permission
--   , xx13Description :: Text
--     -- ^ A short description of the application
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX13 where
--   parseJSON = A.withObject "xx13" $ \v -> do
--     xx13Name <- v A..: "name"
--     xx13IconUrl <- v A..: "icon_url"
--     xx13CallbackUrls <- v A..: "callback_urls"
--     xx13Homepage <- v A..: "homepage"
--     xx13IsTrusted <- v A..: "is_trusted"
--     xx13Description <- v A..: "description"
--     return XX13 { .. }

-- instance A.ToJSON XX13 where
--   toJSON XX13 { .. } = A.object
--     [ "name" A..= xx13Name
--     , "icon_url" A..= xx13IconUrl
--     , "callback_urls" A..= xx13CallbackUrls
--     , "homepage" A..= xx13Homepage
--     , "is_trusted" A..= xx13IsTrusted
--     , "description" A..= xx13Description
--     ]

-- --

-- --

-- data XX15 = XX15
--   { xx15ClientIds :: (Seq Text)
--   , xx15FileInfos :: (Seq FileInfo)
--     -- ^ A list of file metadata that has been stored in the database
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX15 where
--   parseJSON = A.withObject "xx15" $ \v -> do
--     xx15ClientIds <- v A..: "client_ids"
--     xx15FileInfos <- v A..: "file_infos"
--     return XX15 { .. }

-- instance A.ToJSON XX15 where
--   toJSON XX15 { .. } = A.object
--     [ "client_ids" A..= xx15ClientIds
--     , "file_infos" A..= xx15FileInfos
--     ]

-- --

-- --

-- data XX17 = XX17
--   { xx17MarkUnread :: Text
--   , xx17Desktop :: Text
--     -- ^ Controls when to send desktop notifications. The possible options are `default`, `all`, `mention`, `none`
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX17 where
--   parseJSON = A.withObject "xx17" $ \v -> do
--     xx17MarkUnread <- v A..: "mark_unread"
--     xx17Desktop <- v A..: "desktop"
--     return XX17 { .. }

-- instance A.ToJSON XX17 where
--   toJSON XX17 { .. } = A.object
--     [ "mark_unread" A..= xx17MarkUnread
--     , "desktop" A..= xx17Desktop
--     ]

-- --

-- --

-- data XX19 = XX19
--   { xx19LdapId :: Text
--   , xx19CurrentService :: Text
--     -- ^ The service the user currently uses to login
--   , xx19NewService :: Text
--     -- ^ The service the user will use to login
--   , xx19Password :: Text
--     -- ^ The password used with the current service
--   , xx19Email :: Text
--     -- ^ The email of the user
--   , xx19MfaCode :: Text
--     -- ^ The MFA code of the current service
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX19 where
--   parseJSON = A.withObject "xx19" $ \v -> do
--     xx19LdapId <- v A..: "ldap_id"
--     xx19CurrentService <- v A..: "current_service"
--     xx19NewService <- v A..: "new_service"
--     xx19Password <- v A..: "password"
--     xx19Email <- v A..: "email"
--     xx19MfaCode <- v A..: "mfa_code"
--     return XX19 { .. }

-- instance A.ToJSON XX19 where
--   toJSON XX19 { .. } = A.object
--     [ "ldap_id" A..= xx19LdapId
--     , "current_service" A..= xx19CurrentService
--     , "new_service" A..= xx19NewService
--     , "password" A..= xx19Password
--     , "email" A..= xx19Email
--     , "mfa_code" A..= xx19MfaCode
--     ]

-- --

-- data XX2 = XX2
--   { xx2Username :: Text
--   , xx2Gender :: Text
--   , xx2FirstName :: Text
--   , xx2LastName :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX2 where
--   parseJSON = A.withObject "xx2" $ \v -> do
--     xx2Username <- v A..: "username"
--     xx2Gender <- v A..: "gender"
--     xx2FirstName <- v A..: "first_name"
--     xx2LastName <- v A..: "last_name"
--     return XX2 { .. }

-- instance A.ToJSON XX2 where
--   toJSON XX2 { .. } = A.object
--     [ "username" A..= xx2Username
--     , "gender" A..= xx2Gender
--     , "first_name" A..= xx2FirstName
--     , "last_name" A..= xx2LastName
--     ]

-- --

-- data XX20 = XX20
--   { xx20Type :: Text
--   , xx20Data :: UnknownType
--     -- ^ An object containing any additional data required for this job type
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX20 where
--   parseJSON = A.withObject "xx20" $ \v -> do
--     xx20Type <- v A..: "type"
--     xx20Data <- v A..: "data"
--     return XX20 { .. }

-- instance A.ToJSON XX20 where
--   toJSON XX20 { .. } = A.object
--     [ "type" A..= xx20Type
--     , "data" A..= xx20Data
--     ]

--

-- --

-- data XX22 = XX22
--   { xx22Code :: Text
--   , xx22Activate :: UnknownType
--     -- ^ Use `true` to activate, `false` to deactivate
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX22 where
--   parseJSON = A.withObject "xx22" $ \v -> do
--     xx22Code <- v A..: "code"
--     xx22Activate <- v A..: "activate"
--     return XX22 { .. }

-- instance A.ToJSON XX22 where
--   toJSON XX22 { .. } = A.object
--     [ "code" A..= xx22Code
--     , "activate" A..= xx22Activate
--     ]

-- --

-- data XX23 = XX23
--   { xx23DisplayName :: Text
--   , xx23CompanyName :: Text
--   , xx23InviteId :: Text
--   , xx23Description :: Text
--   , xx23AllowOpenInvite :: UnknownType
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX23 where
--   parseJSON = A.withObject "xx23" $ \v -> do
--     xx23DisplayName <- v A..: "display_name"
--     xx23CompanyName <- v A..: "company_name"
--     xx23InviteId <- v A..: "invite_id"
--     xx23Description <- v A..: "description"
--     xx23AllowOpenInvite <- v A..: "allow_open_invite"
--     return XX23 { .. }

-- instance A.ToJSON XX23 where
--   toJSON XX23 { .. } = A.object
--     [ "display_name" A..= xx23DisplayName
--     , "company_name" A..= xx23CompanyName
--     , "invite_id" A..= xx23InviteId
--     , "description" A..= xx23Description
--     , "allow_open_invite" A..= xx23AllowOpenInvite
--     ]

-- --

-- data XX24 = XX24
--   { xx24NewPassword :: Text
--   , xx24Code :: Text
--     -- ^ The recovery code
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX24 where
--   parseJSON = A.withObject "xx24" $ \v -> do
--     xx24NewPassword <- v A..: "new_password"
--     xx24Code <- v A..: "code"
--     return XX24 { .. }

-- instance A.ToJSON XX24 where
--   toJSON XX24 { .. } = A.object
--     [ "new_password" A..= xx24NewPassword
--     , "code" A..= xx24Code
--     ]

-- --

-- data XX25 = XX25
--   { xx25ChannelId :: Text
--   , xx25DisplayName :: Text
--     -- ^ The display name for this incoming webhook
--   , xx25Description :: Text
--     -- ^ The description for this incoming webhook
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX25 where
--   parseJSON = A.withObject "xx25" $ \v -> do
--     xx25ChannelId <- v A..: "channel_id"
--     xx25DisplayName <- v A..: "display_name"
--     xx25Description <- v A..: "description"
--     return XX25 { .. }

-- instance A.ToJSON XX25 where
--   toJSON XX25 { .. } = A.object
--     [ "channel_id" A..= xx25ChannelId
--     , "display_name" A..= xx25DisplayName
--     , "description" A..= xx25Description
--     ]

-- --

-- data XX26 = XX26
--   { xx26Message :: Text
--   , xx26Level :: Text
--     -- ^ The error level, e.g. ERROR or INFO
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX26 where
--   parseJSON = A.withObject "xx26" $ \v -> do
--     xx26Message <- v A..: "message"
--     xx26Level <- v A..: "level"
--     return XX26 { .. }

-- instance A.ToJSON XX26 where
--   toJSON XX26 { .. } = A.object
--     [ "message" A..= xx26Message
--     , "level" A..= xx26Level
--     ]

-- --

-- data XX28 = XX28
--   { xx28Header :: Text
--   , xx28DisplayName :: Text
--     -- ^ The non-unique UI name for the channel
--   , xx28Name :: Text
--     -- ^ The unique handle for the channel, will be present in the channel URL
--   , xx28Type :: Text
--     -- ^ 'O' for a public channel, 'P' for a private channel
--   , xx28Id :: Text
--     -- ^ The channel's id, not updatable
--   , xx28Purpose :: Text
--     -- ^ A short description of the purpose of the channel
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX28 where
--   parseJSON = A.withObject "xx28" $ \v -> do
--     xx28Header <- v A..: "header"
--     xx28DisplayName <- v A..: "display_name"
--     xx28Name <- v A..: "name"
--     xx28Type <- v A..: "type"
--     xx28Id <- v A..: "id"
--     xx28Purpose <- v A..: "purpose"
--     return XX28 { .. }

-- instance A.ToJSON XX28 where
--   toJSON XX28 { .. } = A.object
--     [ "header" A..= xx28Header
--     , "display_name" A..= xx28DisplayName
--     , "name" A..= xx28Name
--     , "type" A..= xx28Type
--     , "id" A..= xx28Id
--     , "purpose" A..= xx28Purpose
--     ]

-- --

-- data XX29 = XX29
--   { xx29CurrentPassword :: Text
--   , xx29NewPassword :: Text
--     -- ^ The new password for the user
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX29 where
--   parseJSON = A.withObject "xx29" $ \v -> do
--     xx29CurrentPassword <- v A..: "current_password"
--     xx29NewPassword <- v A..: "new_password"
--     return XX29 { .. }

-- instance A.ToJSON XX29 where
--   toJSON XX29 { .. } = A.object
--     [ "current_password" A..= xx29CurrentPassword
--     , "new_password" A..= xx29NewPassword
--     ]

-- --

-- data XX3 = XX3
--   { xx3Url :: Text
--   , xx3Width :: UnknownType
--   , xx3SecureUrl :: Text
--   , xx3Type :: Text
--   , xx3Height :: UnknownType
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX3 where
--   parseJSON = A.withObject "xx3" $ \v -> do
--     xx3Url <- v A..: "url"
--     xx3Width <- v A..: "width"
--     xx3SecureUrl <- v A..: "secure_url"
--     xx3Type <- v A..: "type"
--     xx3Height <- v A..: "height"
--     return XX3 { .. }

-- instance A.ToJSON XX3 where
--   toJSON XX3 { .. } = A.object
--     [ "url" A..= xx3Url
--     , "width" A..= xx3Width
--     , "secure_url" A..= xx3SecureUrl
--     , "type" A..= xx3Type
--     , "height" A..= xx3Height
--     ]

-- --

-- data XX30 = XX30
--   { xx30Username :: Text
--   , xx30FirstName :: Text
--   , xx30LastName :: Text
--   , xx30NotifyProps :: UnknownType
--   , xx30Locale :: Text
--   , xx30Id :: Text
--   , xx30Props :: UnknownType
--   , xx30Position :: Text
--   , xx30Nickname :: Text
--   , xx30Email :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX30 where
--   parseJSON = A.withObject "xx30" $ \v -> do
--     xx30Username <- v A..: "username"
--     xx30FirstName <- v A..: "first_name"
--     xx30LastName <- v A..: "last_name"
--     xx30NotifyProps <- v A..: "notify_props"
--     xx30Locale <- v A..: "locale"
--     xx30Id <- v A..: "id"
--     xx30Props <- v A..: "props"
--     xx30Position <- v A..: "position"
--     xx30Nickname <- v A..: "nickname"
--     xx30Email <- v A..: "email"
--     return XX30 { .. }

-- instance A.ToJSON XX30 where
--   toJSON XX30 { .. } = A.object
--     [ "username" A..= xx30Username
--     , "first_name" A..= xx30FirstName
--     , "last_name" A..= xx30LastName
--     , "notify_props" A..= xx30NotifyProps
--     , "locale" A..= xx30Locale
--     , "id" A..= xx30Id
--     , "props" A..= xx30Props
--     , "position" A..= xx30Position
--     , "nickname" A..= xx30Nickname
--     , "email" A..= xx30Email
--     ]

-- --

-- data XX31 = XX31
--   { xx31Username :: Text
--   , xx31FirstName :: Text
--   , xx31LastName :: Text
--   , xx31Locale :: Text
--   , xx31Props :: UnknownType
--   , xx31Password :: Text
--   , xx31Nickname :: Text
--   , xx31Email :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX31 where
--   parseJSON = A.withObject "xx31" $ \v -> do
--     xx31Username <- v A..: "username"
--     xx31FirstName <- v A..: "first_name"
--     xx31LastName <- v A..: "last_name"
--     xx31Locale <- v A..: "locale"
--     xx31Props <- v A..: "props"
--     xx31Password <- v A..: "password"
--     xx31Nickname <- v A..: "nickname"
--     xx31Email <- v A..: "email"
--     return XX31 { .. }

-- instance A.ToJSON XX31 where
--   toJSON XX31 { .. } = A.object
--     [ "username" A..= xx31Username
--     , "first_name" A..= xx31FirstName
--     , "last_name" A..= xx31LastName
--     , "locale" A..= xx31Locale
--     , "props" A..= xx31Props
--     , "password" A..= xx31Password
--     , "nickname" A..= xx31Nickname
--     , "email" A..= xx31Email
--     ]

-- --

-- data XX32 = XX32
--   { xx32Url :: Text
--   , xx32TeamId :: Text
--     -- ^ Team ID to where the command should be created
--   , xx32Trigger :: Text
--     -- ^ Activation word to trigger the command
--   , xx32Method :: Text
--     -- ^ `'P'` for post request, `'G'` for get request
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX32 where
--   parseJSON = A.withObject "xx32" $ \v -> do
--     xx32Url <- v A..: "url"
--     xx32TeamId <- v A..: "team_id"
--     xx32Trigger <- v A..: "trigger"
--     xx32Method <- v A..: "method"
--     return XX32 { .. }

-- instance A.ToJSON XX32 where
--   toJSON XX32 { .. } = A.object
--     [ "url" A..= xx32Url
--     , "team_id" A..= xx32TeamId
--     , "trigger" A..= xx32Trigger
--     , "method" A..= xx32Method
--     ]

-- --

-- data XX33 = XX33
--   { xx33TriggerWhen :: Integer
--   , xx33DisplayName :: Text
--     -- ^ The display name for this outgoing webhook
--   , xx33Description :: Text
--     -- ^ The description for this outgoing webhook
--   , xx33ChannelId :: Text
--     -- ^ The ID of a public channel that the webhook watchs
--   , xx33TeamId :: Text
--     -- ^ The ID of the team that the webhook watchs
--   , xx33ContentType :: Text
--     -- ^ The format to POST the data in, either `application/json` or `application/x-www-form-urlencoded`
--   , xx33TriggerWords :: (Seq Text)
--     -- ^ List of words for the webhook to trigger on
--   , xx33CallbackUrls :: (Seq Text)
--     -- ^ The URLs to POST the payloads to when the webhook is triggered
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX33 where
--   parseJSON = A.withObject "xx33" $ \v -> do
--     xx33TriggerWhen <- v A..: "trigger_when"
--     xx33DisplayName <- v A..: "display_name"
--     xx33Description <- v A..: "description"
--     xx33ChannelId <- v A..: "channel_id"
--     xx33TeamId <- v A..: "team_id"
--     xx33ContentType <- v A..: "content_type"
--     xx33TriggerWords <- v A..: "trigger_words"
--     xx33CallbackUrls <- v A..: "callback_urls"
--     return XX33 { .. }

-- instance A.ToJSON XX33 where
--   toJSON XX33 { .. } = A.object
--     [ "trigger_when" A..= xx33TriggerWhen
--     , "display_name" A..= xx33DisplayName
--     , "description" A..= xx33Description
--     , "channel_id" A..= xx33ChannelId
--     , "team_id" A..= xx33TeamId
--     , "content_type" A..= xx33ContentType
--     , "trigger_words" A..= xx33TriggerWords
--     , "callback_urls" A..= xx33CallbackUrls
--     ]

-- --

-- data XX34 = XX34
--   { xx34DisplayName :: Text
--   , xx34Description :: Text
--   , xx34Name :: Text
--   , xx34Id :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX34 where
--   parseJSON = A.withObject "xx34" $ \v -> do
--     xx34DisplayName <- v A..: "display_name"
--     xx34Description <- v A..: "description"
--     xx34Name <- v A..: "name"
--     xx34Id <- v A..: "id"
--     return XX34 { .. }

-- instance A.ToJSON XX34 where
--   toJSON XX34 { .. } = A.object
--     [ "display_name" A..= xx34DisplayName
--     , "description" A..= xx34Description
--     , "name" A..= xx34Name
--     , "id" A..= xx34Id
--     ]

-- --

-- data XX35 = XX35
--   { xx35HookId :: Text
--   , xx35ChannelId :: Text
--     -- ^ The ID of a public channel or private group that receives the webhook payloads.
--   , xx35DisplayName :: Text
--     -- ^ The display name for this incoming webhook
--   , xx35Description :: Text
--     -- ^ The description for this incoming webhook
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX35 where
--   parseJSON = A.withObject "xx35" $ \v -> do
--     xx35HookId <- v A..: "hook_id"
--     xx35ChannelId <- v A..: "channel_id"
--     xx35DisplayName <- v A..: "display_name"
--     xx35Description <- v A..: "description"
--     return XX35 { .. }

-- instance A.ToJSON XX35 where
--   toJSON XX35 { .. } = A.object
--     [ "hook_id" A..= xx35HookId
--     , "channel_id" A..= xx35ChannelId
--     , "display_name" A..= xx35DisplayName
--     , "description" A..= xx35Description
--     ]

-- --

-- data XX36 = XX36
--   { xx36DisplayName :: Text
--   , xx36AllowedDomains :: Text
--   , xx36CompanyName :: Text
--   , xx36AllowOpenInvite :: Text
--   , xx36InviteId :: Text
--   , xx36Description :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX36 where
--   parseJSON = A.withObject "xx36" $ \v -> do
--     xx36DisplayName <- v A..: "display_name"
--     xx36AllowedDomains <- v A..: "allowed_domains"
--     xx36CompanyName <- v A..: "company_name"
--     xx36AllowOpenInvite <- v A..: "allow_open_invite"
--     xx36InviteId <- v A..: "invite_id"
--     xx36Description <- v A..: "description"
--     return XX36 { .. }

-- instance A.ToJSON XX36 where
--   toJSON XX36 { .. } = A.object
--     [ "display_name" A..= xx36DisplayName
--     , "allowed_domains" A..= xx36AllowedDomains
--     , "company_name" A..= xx36CompanyName
--     , "allow_open_invite" A..= xx36AllowOpenInvite
--     , "invite_id" A..= xx36InviteId
--     , "description" A..= xx36Description
--     ]

-- --

-- data XX37 = XX37
--   { xx37Secret :: Text
--   , xx37QrCode :: Text
--     -- ^ A base64 encoded QR code image
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX37 where
--   parseJSON = A.withObject "xx37" $ \v -> do
--     xx37Secret <- v A..: "secret"
--     xx37QrCode <- v A..: "qr_code"
--     return XX37 { .. }

-- instance A.ToJSON XX37 where
--   toJSON XX37 { .. } = A.object
--     [ "secret" A..= xx37Secret
--     , "qr_code" A..= xx37QrCode
--     ]

-- --

-- data XX38 = XX38
--   { xx38Username :: Text
--   , xx38FirstName :: Text
--   , xx38LastName :: Text
--   , xx38NotifyProps :: UnknownType
--   , xx38Locale :: Text
--   , xx38Props :: UnknownType
--   , xx38Position :: Text
--   , xx38Nickname :: Text
--   , xx38Email :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX38 where
--   parseJSON = A.withObject "xx38" $ \v -> do
--     xx38Username <- v A..: "username"
--     xx38FirstName <- v A..: "first_name"
--     xx38LastName <- v A..: "last_name"
--     xx38NotifyProps <- v A..: "notify_props"
--     xx38Locale <- v A..: "locale"
--     xx38Props <- v A..: "props"
--     xx38Position <- v A..: "position"
--     xx38Nickname <- v A..: "nickname"
--     xx38Email <- v A..: "email"
--     return XX38 { .. }

-- instance A.ToJSON XX38 where
--   toJSON XX38 { .. } = A.object
--     [ "username" A..= xx38Username
--     , "first_name" A..= xx38FirstName
--     , "last_name" A..= xx38LastName
--     , "notify_props" A..= xx38NotifyProps
--     , "locale" A..= xx38Locale
--     , "props" A..= xx38Props
--     , "position" A..= xx38Position
--     , "nickname" A..= xx38Nickname
--     , "email" A..= xx38Email
--     ]

-- --

-- data XX4 = XX4
--   { xx4Username :: Text
--   , xx4Gender :: Text
--   , xx4FirstName :: Text
--   , xx4LastName :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX4 where
--   parseJSON = A.withObject "xx4" $ \v -> do
--     xx4Username <- v A..: "username"
--     xx4Gender <- v A..: "gender"
--     xx4FirstName <- v A..: "first_name"
--     xx4LastName <- v A..: "last_name"
--     return XX4 { .. }

-- instance A.ToJSON XX4 where
--   toJSON XX4 { .. } = A.object
--     [ "username" A..= xx4Username
--     , "gender" A..= xx4Gender
--     , "first_name" A..= xx4FirstName
--     , "last_name" A..= xx4LastName
--     ]

-- --

-- data XX5 = XX5
--   { xx5Url :: Text
--   , xx5SecureUrl :: Text
--   , xx5Type :: Text
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX5 where
--   parseJSON = A.withObject "xx5" $ \v -> do
--     xx5Url <- v A..: "url"
--     xx5SecureUrl <- v A..: "secure_url"
--     xx5Type <- v A..: "type"
--     return XX5 { .. }

-- instance A.ToJSON XX5 where
--   toJSON XX5 { .. } = A.object
--     [ "url" A..= xx5Url
--     , "secure_url" A..= xx5SecureUrl
--     , "type" A..= xx5Type
--     ]

-- --

-- --

-- data XX8 = XX8
--   { xx8Token :: Text
--   , xx8TurnUsername :: Text
--     -- ^ The username to use with the TURN server
--   , xx8TurnUri :: Text
--     -- ^ The URI to the TURN server
--   , xx8StunUri :: Text
--     -- ^ The URI to the STUN server
--   , xx8GatewayUrl :: Text
--     -- ^ The URL to the gateway server
--   , xx8TurnPassword :: Text
--     -- ^ The password to use with the TURN server
--   } deriving (Read, Show, Eq)

-- instance A.FromJSON XX8 where
--   parseJSON = A.withObject "xx8" $ \v -> do
--     xx8Token <- v A..: "token"
--     xx8TurnUsername <- v A..: "turn_username"
--     xx8TurnUri <- v A..: "turn_uri"
--     xx8StunUri <- v A..: "stun_uri"
--     xx8GatewayUrl <- v A..: "gateway_url"
--     xx8TurnPassword <- v A..: "turn_password"
--     return XX8 { .. }

-- instance A.ToJSON XX8 where
--   toJSON XX8 { .. } = A.object
--     [ "token" A..= xx8Token
--     , "turn_username" A..= xx8TurnUsername
--     , "turn_uri" A..= xx8TurnUri
--     , "stun_uri" A..= xx8StunUri
--     , "gateway_url" A..= xx8GatewayUrl
--     , "turn_password" A..= xx8TurnPassword
--     ]


-- * Helpers

-- | Add a post to a user's flagged post list. This is a convenience
-- wrapper for the 'mmSaveUsersPreferences' function.
--
--   /Permissions/: Must be logged in as the user being updated or have the
--   @edit_other_users@ permission.
mmFlagPost :: UserId -> PostId -> Session -> IO ()
mmFlagPost uId pId =
  let body = FlaggedPost
        { flaggedPostUserId = uId
        , flaggedPostId     = pId
        , flaggedPostStatus = True
        }
  in inPut (printf "/users/%s/preferences" uId) (jsonBody [body]) noResponse

-- | Remove a post from a user's flagged post list. This is a convenience
-- wrapper for the 'mmSaveUsersPreferences' function.
--
--   /Permissions/: Must be logged in as the user being updated or have the
--   @edit_other_users@ permission.
mmUnflagPost :: UserId -> PostId -> Session -> IO ()
mmUnflagPost uId pId =
  let body = FlaggedPost
        { flaggedPostUserId = uId
        , flaggedPostId     = pId
        , flaggedPostStatus = False
        }
  in inPost (printf "/users/%s/preferences/delete" uId) (jsonBody [body]) noResponse
