# Revision history for mattermost-api

30701.2.0
=========

API changes:

* The `Network.Mattermost.Types` module is now directly exported
  and all clients should obtain their types from this import.  The
  types are still exported from `Network.Mattermost` to allow time
  for this change but this export is deprecated will be removed
  in a future version.

30701.1.0
=========

API changes:
* The `Token` type has been replaced with a `Session` type,
  representing a combination of a `Token` and a `ConnectionData`
  type. All exposed API functions which require an authenticated
  connection will use this instead. This is a major breaking change,
  but makes the API significantly cleaner.
* Removed `UserProfile` type in favor of single pervasive `User` type.
* Replaced the return type of `mmGetTeamMembers` to use a `TeamMember`
  instead of raw JSON `Value`s.

Package changes:
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

First version. Released on an unsuspecting world.
