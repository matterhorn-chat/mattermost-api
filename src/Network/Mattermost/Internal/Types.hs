-- | The types defined in this module are exported to facilitate
-- efforts such as QuickCheck and other instrospection efforts, but
-- users are advised to avoid using these types wherever possible:
-- they can be used in a manner that would cause significant
-- disruption and may be subject to change without being reflected in
-- the mattermost-api version.

module Network.Mattermost.Internal.Types where


data Token = Token String
  deriving (Read, Show, Eq, Ord)

getTokenString :: Token -> String
getTokenString (Token s) = s
