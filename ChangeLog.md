# Revision history for mattermost-api

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
