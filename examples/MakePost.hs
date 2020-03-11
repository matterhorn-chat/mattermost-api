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

import           Network.Mattermost.Endpoints
import           Network.Mattermost.Types
import           Network.Mattermost.Util

import           Config
import           LocalConfig -- You will need to define a function:
                             -- getConfig :: IO Config
                             -- See Config.hs


data Options
  = Options
  { optChannel :: UserText
  , optVerbose :: Bool
  , optMessage :: T.Text
  } deriving (Read, Show)

defaultOptions :: Options
defaultOptions = Options
  { optChannel = UserText "town-square"
  , optVerbose = False
  , optMessage = ""
  }

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "c" ["channel"]
      (ReqArg
        (\arg opt -> return opt { optChannel = UserText . T.pack $ arg })
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
  cd <- initConnectionData (configHostname config) (fromIntegral (configPort config))
                           (ConnectHTTPS True) defaultConnectionPoolConfig

  let login = Login { username = configUsername config
                    , password = configPassword config
                    }

  (session, mmUser) <- join (hoistE <$> mmLogin cd login)
  when (optVerbose opts) $ do
    putStrLn "Authenticated as:"
    pPrint mmUser

  teams <- mmGetUsersTeams UserMe session
  when (optVerbose opts) $ do
    pPrint teams
  forM_ teams $ \t -> do
    when (teamName t == configTeam config) $ do
      users <- mmGetUsers (defaultUserQuery { userQueryInTeam = Just (getId t)
                                            , userQueryPerPage = Just 1000
                                            }) session
      let userMap = HM.fromList [ (getId u, u) | u <- toList users ]
      when (optVerbose opts) $ do
        pPrint userMap
      chans <- mmGetChannelsForUser UserMe (getId t) session
      forM_ chans $ \chan -> do
        when (optVerbose opts) $ do
          pPrint chan
        when (channelName chan == optChannel opts) $ do
          when (not (T.null (optMessage opts))) $ do
            let pendingPost = RawPost
                  { rawPostChannelId = getId chan
                  , rawPostMessage   = optMessage opts
                  , rawPostFileIds   = mempty
                  , rawPostRootId    = Nothing
                  }
            -- pendingPost <- mkPendingPost (optMessage opts)
            --                              (getId mmUser)
            --                              (getId chan)
            post <- mmCreatePost pendingPost session
            when (optVerbose opts) (pPrint post)
