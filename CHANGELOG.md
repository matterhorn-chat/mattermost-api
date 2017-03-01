# Revision history for mattermost-api

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
