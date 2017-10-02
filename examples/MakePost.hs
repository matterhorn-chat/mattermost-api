{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- Note: See LocalConfig.hs to configure example for your server
module Main (main) where
import           Text.Read ( readMaybe )
import           Text.Printf ( printf )
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
import           Network.Mattermost.Util

import           Config
import           LocalConfig -- You will need to define a function:
                             -- getConfig :: IO Config
                             -- See Config.hs


data Options
  = Options
  { optChannel :: T.Text
  , optVerbose :: Bool
  , optMessage :: T.Text
  } deriving (Read, Show)

defaultOptions :: Options
defaultOptions = Options
  { optChannel = "town-square"
  , optVerbose = False
  , optMessage = ""
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
  , Option "m" ["message"]
      (ReqArg
        (\arg opt -> return opt { optMessage = T.pack arg })
        "MESSAGE")
      "message to send"
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
  let cd      = mkConnectionData (configHostname config)
                                 (fromIntegral (configPort config))
                                 ctx
      login   = Login { username = configUsername config
                      , password = configPassword config
                      }

  (session, mmUser) <- join (hoistE <$> mmLogin cd login)
  when (optVerbose opts) $ do
    putStrLn "Authenticated as:"
    pPrint mmUser

  i <- mmGetInitialLoad session
  when (optVerbose opts) $ do
    pPrint i
  forM_ (initialLoadTeams i) $ \t -> do
    when (teamName t == configTeam config) $ do
      userMap <- mmGetProfiles (getId t) 0 10000 session
      when (optVerbose opts) $ do
        pPrint userMap
      chans <- mmGetChannels (getId t) session
      forM_ chans $ \chan -> do
        when (optVerbose opts) $ do
          pPrint chan
        when (channelName chan == optChannel opts) $ do
          when (not (T.null (optMessage opts))) $ do
            pendingPost <- mkPendingPost (optMessage opts)
                                         (getId mmUser)
                                         (getId chan)
            post <- mmPost (getId t) pendingPost session
            when (optVerbose opts) (pPrint post)
