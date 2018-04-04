
40800.0.1
=========

 * `createPool` now requires a secure connection argument. Previously it
   always defaulted to `True` which broke insecure connection setups.
 * Removed the upper bound from `memory` to support GHC 8.4.

40800.0.0
=========

 * Fixed the URI for the mmUnflagPost API call.
 * Some JSON instances now more precisely handle missing optional
   fields.
 * mattermost-api now supports connection pooling with persistent server
   connections. A connection pool configuration is required to connect
   to Mattermost servers (thanks to Abhinav Sarkar)

40700.0.0
=========

API changes:
 * Added a websocket event constructor for the `delete_team` event added
   in 4.7.

40600.1.0
=========

API changes:
 * The `UserSearch` type now wraps optional fields with `Maybe`. The
   field types changed from `Text` to the appropriate ID type.
 * Removed defunct `Network.Mattermost.Types.mkSession` function.
 * Added an API endpoint function `mmGetUsersByIds` for getting users by
   ID.

Other changes:
 * The `doRequest` function used by APIv4 endpoint functions now invokes
   the connection's logger to log request method and URI.

40600.0.0
=========

API changes:
 * This release provides implementations of many of Mattermost's version
   4 API endpoints in the new Network.Mattermost.Endpoints module. The
   old version 3 API is still provided by Network.Mattermost but will
   be deprecated in a future release, and removed after that. Note
   that this release chiefly exposes version 4 endpoints used by the
   Matterhorn application; endpoints not used there have not been tested
   and so are left commented out in this release and may be enabled in
   future releases as needed.
 * This release also provides first-class Haskell types corresponding to
   the server-side configuration; see Network.Mattermost.Types.Config.
 * The Post data type got a new postEditAt field corresponding to the
   (at the time) undocumented `edit_at` field of posts.
 * Websockets got action support; see
   Network.Mattermost.WebSocket.mmSendWSAction and the new
   WebsocketAction type (thanks to Abhinav Sarkar for this change)
 * All UTCTimes received from the Mattermost server are wrapped in a
   `ServerTime` newtype to ensure that users cannot accidentally compare
   such timestamps to local UTCTimes, since the server time is not
   necessarily comparable.
 * Added Network.Mattermost.Endpoints.mmGetClientConfig to get the
   client configuration from the server (Thanks to Kelly McLaughlin for
   this change)

40400.0.0
=========

API changes:
 * Added endpoints for some preference endpoints, group channels, and
   post search:
   * `mmChannelRemoveUser`
   * `mmCreateGroupChannel`
   * `mmSearchPosts`
   * `mmSetPreferences`
 * Added `GroupChannelPreferences` type for showing/hiding group
   channels
 * Separated the previous `NotifyProps` type into `UserNotifyProps`
   and `ChannelNotifyProps`.
 * Websocket parse failures are now captured and appear to the library
   user as an `Either String WebsocketEvent`, where the `String`
   represent a parse failure.
 * Add support for `user_role_updated` websocket event type.

40000.1.0
=========

API changes:
 * The postRootId of Post is now a Maybe PostId to better reflect the
   actual wire format.
 * MinCommand now has fields for reply parent and root post IDs to
   support replying to posts with commands such as /me.
 * CommandResponse's commandResponseType is now Maybe to permit optional
   types.
 * PreferenceCategory got a new constructor, PreferenceCategoryLast,
   mapping to the "last" preference category.
 * Added functions for bulk fetching for channel/user data:
   * mmGetAllChannelsForUser
   * mmGetAllChannelDataForUser
   * mmGetAllChannelsWithDataForUser

40000.0.1
=========

API changes:
 * Added support for the `emoji_added` websocket event type.
 * WEData's JSON parser now permits both null and empty channel IDs.

40000.0.0
=========

Package changes:
 * Added lower bound for binary to avoid build failures in the websocket
   package on GHC 7.10 (see https://github.com/jaspervdj/websockets/pull/155)

API changes:
 * Added support for the `channel_viewed` and `channel_updated`
   websocket events

31000.0.0
=========

API changes:
 * Added flagged posts API:
   * Types: FlaggedPost
   * mmGetFlaggedPosts
   * mmFlagPost
   * mmUnflagPost
 * Added preferences API:
   * Types: Preference, PreferenceCategory, PreferenceName,
     PreferenceValue
   * mmDeletePreferences
   * mmSavePreferences
   * mmGetMyPreferences
 * Added PostType for post types (joins, parts, header changes, etc.)
   and changed the type of Post.postType to use this new type.
 * Added a new field WEData.wepMentions so that websocket events can
   include the set of UserIds in a mention set.
 * Made most fields of PostPropAttachment optional.
 * Added a new PostPropAttachment type as defined by Slack and used by
   Mattermost.
 * Allow null for the "fields" field of PostPropAttachment.
 * Extended PostPropAttachment with additional fields to better match
   the upstream spec.

Other changes:
 * Improved the LoginFailureException message format.
 * Refrain from logging passwords when logging login requests.
 * Logging operations seek to the end of the log in case of a shared log
   file.

30802.1.0
=========

This release supports server version 3.8.2.

API changes:
* Made the PendingPost `created_at` field optional. It defaults to 0.
  This behavior is due to Mattermost's support for admins setting the
  creation timestamp to values in the past. A value of zero causes
  the server to use the server's clock to set the creation timestamp.
  Any other value is only permitted for users with administrative
  privileges.
* Moved some types to a new Types.Internal module and exposed that
  module for testing purposes. It should not be used by anyone wanting
  a stable API. For a stable API, see the export list for the Types
  module.

30802.0.0
=========

This release supports server version 3.8.2.

API changes:
* The `Network.Mattermost.Types` module is now directly exported and all
  clients should obtain their types from this import. The types are
  still exported from `Network.Mattermost` to allow time for this change
  but this export is deprecated will be removed in a future version.
* Added the CommandResponse type for the execute endpoint.
* mmGetMoreChannels, mmGetChannelMembers, and mmGetProfiles now take
  limit/offset parameters.
* mmGetFile now supports v4 file-fetching.
* Added new constructors to the WebsocketEventType corresponding to
  server websocket events.
* mmUpdateLastViewedAt was replaced with mmViewChannel.
* Added the WithDefault type to wrap around bools and NotifyOptions.
* Added NotifyProps types.
* The `Token` type has been replaced with a `Session` type,
  representing a combination of a `Token` and a `ConnectionData`
  type. All exposed API functions which require an authenticated
  connection will use this instead. This is a major breaking change,
  but makes the API significantly cleaner.
* Removed `UserProfile` type in favor of single pervasive `User` type.
* Replaced the return type of `mmGetTeamMembers` to use a `TeamMember`
  instead of raw JSON `Value`s.

Documentation:
* All API functions how have corresponding HTTP route documentation.

Package changes:
* Source repository was updated.
* Constrained 'memory' version to avoid 'foundation' dependency.
* Include Network.Mattermost.TH.
* The `Network.Mattermost.Websocket` module now exports everything
  exported by `Network.Mattermost.Websocket.Types` in order to cut
  down on the number of imports needed by users.

30701.0.0
=========

* Supports server version 3.7.1.

API changes:
* Tests now provide websocket event testing infrastructure
* The Channel data type now supports Group channels (type "G")
* Added mmGetTeamMembers to get the users in a channel
* Added support for the Post type `system_header_change` and the post
  properties `new_header` and `old_header` as described at
  https://github.com/mattermost/platform/pull/4209
* Removed the UserProfile type in favor of the User type (fixed #23)

Bug fixes:
* WebSocket.Types: permit empty `team_id` in event data

30600.2.2
=========

Bug fixes:
 * Support optional `notify_props` and `last_password_update` in
   mmGetUser responses.

Package changes:
 * Renamed ChangeLog.md to CHANGELOG.md.

Testing changes:
 * Added support for testing websocket events and updated the test suite
   to check for expected websocket events.

30600.2.1
=========

API changes:
 * Export FileInfo type

Bug fixes:
 * Fixed parsing of nullable width/height fields in FileInfo
 * Fixed parsing of create_at, update_at, and delete_at timestamp fields
   in FileInfo

30600.2.0
=========

API changes:
 * Added mmDeletePost
 * Added mmUpdatePost for editing posts
 * Post: make deletion time optional to match server API, do millisecond
   conversion on JSON encoding
 * PendingPost: add fields for setting parents in case of replies
 * Export PendingPost type so it can be modified for replies and edits

Bug fixes:
 * Post: do millisecond conversion of timestamps on JSON encoding

30600.1.0
=========

API changes:
 * MinCommand lost its unused minComSuggest field

Bug fixes:
 * The JSON format of MinCommand got its channelId field (3.5.0) renamed
   to channel_id (3.6.0). See also:
   https://github.com/mattermost/platform/issues/5281

Other:
 * mmGetJSONBody got a debugging label that it now uses to generate
   exception messages to indicate what kind of value it was attempting
   to parse.

30600.0.0
=========

Initial release for server version 3.6.0.

0.1.0.0
=======

First version.
