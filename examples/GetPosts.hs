{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- Note: See LocalConfig.hs to configure example for your server
module Main (main) where
import           Text.Read ( readMaybe )
import           Text.Printf ( printf )
import           Data.Foldable ( toList )
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Text.Show.Pretty ( pPrint )
import           Network.Connection
import           System.Process ( readProcess )
import           System.Exit ( exitFailure
                             , exitWith
                             , ExitCode(..) )
import           Data.Foldable
import           Control.Monad ( when, join )

import           System.Console.GetOpt
import           System.Environment ( getArgs, getProgName )

import           Network.Mattermost
import           Network.Mattermost.Logging
import           Network.Mattermost.Util

import           Config
import           LocalConfig -- You will need to define a function:
                             -- getConfig :: IO Config
                             -- See Config.hs


data Options
  = Options
  { optChannel :: T.Text
  , optVerbose :: Bool
  , optOffset  :: Int
  , optLimit   :: Int
  , optLogging :: Bool
  } deriving (Read, Show)

defaultOptions :: Options
defaultOptions = Options
  { optChannel = "town-square"
  , optVerbose = False
  , optOffset  = 0
  , optLimit   = 10
  , optLogging = False
  }

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "c" ["channel"]
      (ReqArg
        (\arg opt -> return opt { optChannel = T.pack arg })
        "CHANNEL")
      "Channel to fetch posts from"
  , Option "v" ["verbose"]
      (NoArg
        (\opt -> return opt { optVerbose = True }))
      "Enable verbose output"
  , Option "L" ["logging"]
      (NoArg
        (\opt -> do
            return opt { optLogging = True }))
      "Log debug output to stderr"
  , Option "o" ["offset"]
      (ReqArg
        (\arg opt -> do
          case readMaybe arg of
            Nothing -> do putStrLn "offset must be an int"
                          exitFailure
            Just i  -> return opt { optOffset = i })
        "OFFSET")
      "Starting offset to grab posts, 0 is most recent"
  , Option "l" ["limit"]
      (ReqArg
        (\arg opt -> do
          case readMaybe arg of
            Nothing -> do putStrLn "limit must be an int"
                          exitFailure
            Just n  -> return opt { optLimit = n })
        "LIMIT")
      "Maximum number of posts to fetch"
  , Option "h" ["help"]
      (NoArg
        (\_ -> do
          prg <- getProgName
          putStrLn (usageInfo prg options)
          exitWith ExitSuccess))
      "Show help"
  ]

main :: IO ()
main = do
  args <- getArgs
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions

  config <- getConfig -- see LocalConfig import
  ctx    <- initConnectionContext
  let cd'      = mkConnectionData (T.unpack (configHostname config))
                                 (fromIntegral (configPort config))
                                 ctx
      login   = Login { username = configUsername config
                      , password = configPassword config
                      }
      cd = if optLogging opts
             then cd' `withLogger` mmLoggerDebugErr
             else cd'

  (token, mmUser) <- join (hoistE <$> mmLogin cd login)
  when (optVerbose opts) $ do
    putStrLn "Authenticated as:"
    pPrint mmUser

  i <- mmGetInitialLoad cd token
  when (optVerbose opts) $ do
    pPrint i
  forM_ (initialLoadTeams i) $ \t -> do
    when (teamName t == configTeam config) $ do
      userMap <- mmGetProfiles cd token (getId t)
      when (optVerbose opts) $ do
        pPrint userMap
      Channels chans _md <- mmGetChannels cd token (getId t)
      forM_ chans $ \chan -> do
        when (optVerbose opts) $ do
          pPrint chan
        when (channelName chan == optChannel opts) $ do
          posts <- mmGetPosts cd token (getId t) (getId chan) (optOffset opts) (optLimit opts)
          -- XXX: is the order really reversed?
          forM_ (reverse (toList (postsOrder posts))) $ \postId -> do
            -- this is just a toy program, so we don't care about
            -- this pattern match failure
            let Just p    = HM.lookup postId (postsPosts posts)
                Just uId  = postUserId p
                Just user = HM.lookup uId userMap
            when (optVerbose opts) $ do
               pPrint p
            let message = printf "%s: %s"
                                 (userProfileUsername user)
                                 (postMessage p)
            putStrLn message
