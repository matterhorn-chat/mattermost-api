
50200.9.0
=========

API changes:
 * Added `Network.Mattermost.Types.Internal.connectionDataURL` and its
   return type, `ServerBaseURL`, to obtain base URLs corresponding to a
   connection handle.

50200.8.0
=========

API changes:
 * Added a `WMUnknownEvent` constructor to the `WebsocketEventType` data
   type to hold unknown event types to avoid parse failures for new
   event types.
 * Added `mmListCommandsForTeam` to get commands for a team and
   implemented a JSON parser for the `Command` type.

Package changes:
 * Upgraded to HTTP 4000.3.15 to get an `Eq` instance fix for HTTP
   header name comparisons.

50200.7.0
=========

 * Added `mmUpdateChannelNotifications` and fixed the JSON encoding of
   the `ChannelNotifyProps` type.

50200.6.0
=========

API changes:
 * Version 4 API functions now all have the potential to raise a new
   exception type, `RateLimitExceptions`, when HTTP 429 (rate limiting)
   responses are received. The new exception type provides rate limit
   metadata received from the server so the caller can determine whether
   to attempt a request again, and how long to wait before attempting
   it.
 * The `ChannelNotifyProps` type got a new
   `channelNotifyPropsIgnoreChannelMentions` field.
 * The `WEData` type got a `wepChannelMember` field.

Other changes:
 * `channel_member_updated` websocket events are now parsed.
 * Some haddock syntax errors were fixed (thanks Eric Mertens)

50200.5.0
=========

API changes:
 * Added support for making connections to Mattermost servers that use
   non-root URL paths for their API endpoints. This change adds a new
   field, `cdUrlPath`, to the `ConnectionData` type. It also added a URL
   path argument of type `Text` to `mkConnectionData`. (Thanks Isaiah
   Mindich)
 * Added `Network.Mattermost.Endpoints.mmGetLimitedClientConfiguration`
   (thanks Eric Mertens)

50200.4.0
=========

 * The post pinning API is now supported.
   * Adds a new postPinned field to the Post type to allow parsing
     "is_pinned" values in post editing websocket events. Typically
     "is_pinned" is not present in post structures, in which case this
     will take the value Nothing.
   * Exposes the new StatusOK type that is returned by new API functions.
   * Adds new API functions:
     * mmPinPostToChannel
     * mmUnpinPostToChannel
     * mmGetChannelPinnedPosts

 * The library now supports connecting to HTTPS endpoints without valid
   certificates.

   This change gets rid of the initConnectionDataInsecure function
   and instead makes initConnectionData take a new argument of type
   'ConnectionType' that describes how the connection should be made.

   The ConnectionType type indicates that a connection should be HTTP,
   HTTPS with cert validation, or HTTPS without cert validation.

   This change also modified ConnectionData to carry the connecion type
   rather than just a Bool indicating HTTP/HTTPS.

 * Websocket action responses are now parsed properly.

   Websocket actions, such as "user typing" notifications, generate
   server responses that we previously could not parse. We weren't
   parsing them because the parser for websocket events wasn't aware
   of an alternative response message structure. This patch adds a new
   type for action responses. It adds a new type rather than adding a
   new constructor to the event type because I think that's a less ugly
   API (the API in this patch is not ideal). This patch also adds logic
   to the websocket response parser to first attempt to parse incoming
   messages as websocket events (the most common case) and then fall
   back to attempting a parse as a websocket action response. If both
   fail, the message and the parse exceptions are all logged.

50200.3.0
=========

Bug fixes:
 * Fixed a bug in the Team JSON decoder where the `invite_id`
   field was required despite it truly being optional (see also
   https://github.com/matterhorn-chat/matterhorn/issues/550)

50200.2.0
=========

API changes:
 * Added `Network.Mattermost.Endpoints.mmDeleteReaction` to delete
   reactions.
 * Added `Network.Mattermost.Endpoints.mmPostReaction` to add reactions.
 * Added an `Emoji` data type and functions for custom emoji search and listing:
   * `Network.Mattermost.Endpoints.mmSearchCustomEmoji`
   * `Network.Mattermost.Endpoints.mmGetListOfCustomEmoji`

New features:
 * Add support for SOCKS proxies via standard environment variables
   `HTTPS_PROXY` and `ALL_PROXY`.
 * Support `NO_PROXY` to blacklist proxied hosts.

50200.1.4
=========

Bug fixes:
 * Content-type headers in server API responses are now parsed correctly
   in the presence of media type parameters as per RFC 2616.

50200.1.3
=========

API changes:
 * The `PostProps` type got support for the `from_webhook` prop

50200.1.2
=========

Bug fixes:
 * The server response to an uploaded file may report "null" for
   client_ids.  This previously failed the parse attempt to create a
   `Seq ClientId`; this fix updates to conver null to an empty `Seq`.
   This typically manifested in attempting to attach a file in a
   common channel.

Compatibility:
 * Updated supported/tested GHC versions: 8.2.2, 8.4.4, and 8.6.3.

50200.1.1
=========

Bug fixes:
 * The `userCreateAt`, `userUpdateAt`, and `userAuthData` fields of
   `User` are now permitted to be optional in the user JSON encoding to
   be consistent with the server.

50200.1.0
=========

 * Added `mmUploadFile` and exposed its `UploadResponse` result type.
 * Fixed the `FileInfo` JSON decoder to permit `post_id` to be omitted.
 * Added `RestrictDirectMessageSetting` data type for
   `clientConfigRestrictDirectMessage` field of `ClientConfig`.
 * Added `mmAutocompleteUsers`.
 * Added `mmAutocompleteChannels`.
 * Added `DirectChannelShowStatus` preference type and constructor.

Bug fixes:
 * `mkQueryString` now properly URI-encodes keys and values.

50200.0.1
=========

Bug fixes:
 * Fix the specification of the QuerySince time for fetching channel
   posts.  The previous version generated a user-displayable date-time
   which contained spaces and created a malformed URL as well as not
   being in the proper format for the server.

50200.0.0
=========

API changes:
 * Expose new type, TeammateNameDisplayMode, as the type of the
   clientConfigTeammateNameDisplay field
 * Removed various fields from TeamSettings and ClientConfig that seem
   to have been removed in 4.9
 * Remove `parent_id` fields from Post and PendingPost since they are
   unused and thus confusing
 * Added basic parsing support for websocket events new in 5.2 (fixes #408)
 * ClientConfig: removed EnableUserCreation field that is removed in 5.0
 * Removed `extra_update_at` channel data field

Bug fixes:
 * Fixed examples and tests to use the UserText type instead of
   Data.Text (thanks Carlos D <m@cdagostino.io>)

40900.1.0
=========

API changes:
 * Added a `newtype`, `UserText`, to wrap many API response fields to
   indicate that they may contain unsafe or unsanitized user input. Also
   added `unsafeUserText` to unwrap such values when the caller knows
   that using the unsafe value is appropriate.

40900.0.0
=========

 * Endpoints: enabled `mmPatchPost` and removed duplicate argument type
   for `PostUpdate`.
 * Ensure that file fetching uses the V4 API endpoint.

40800.0.2
=========

 * The websocket connection now uses the V4 API endpoint.
 * submitRequest now also retries on "resource vanished" exceptions.

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
