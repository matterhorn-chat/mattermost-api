{-# LANGUAGE TemplateHaskell #-}

module Network.Mattermost.Version (mmApiVersion) where

import Data.Version (showVersion)
import Development.GitRev (gitBranch, gitHash)
import Paths_mattermost_api (version)

mmApiVersion :: String
mmApiVersion
  | $(gitHash) == ("UNKNOWN" :: String) = "mattermost-api " ++ showVersion version
  | otherwise = "mattermost-api " ++ showVersion version ++ " (" ++
                $(gitBranch) ++ "@" ++ take 7 $(gitHash) ++ ")"
