{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import           Control.Exception as E
import           Control.Concurrent ( myThreadId )
import           Control.Monad ( when, join, forever )
import           Data.Bits (xor)
import           Data.Char (ord)
import           Data.Word (Word8)
import           Data.List ( sort, isPrefixOf )
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import           Network.Connection
import           Text.Read ( readMaybe )
import           Text.Show.Pretty ( pPrint )

import           System.Console.GetOpt
import           System.Environment ( getArgs, getProgName )
import           System.Exit ( exitFailure
                             , exitWith
                             , ExitCode(..) )

import           Network.Mattermost
import           Network.Mattermost.Util
import           Network.Mattermost.WebSocket
import           Network.Mattermost.WebSocket.Types

import           Config
import           LocalConfig -- You will need to define a function:
                             -- getConfig :: IO Config
                             -- See Config.hs

data Options
  = Options
  { optVerbose :: Bool
  } deriving (Read, Show)

defaultOptions :: Options
defaultOptions = Options
  { optVerbose = False
  }

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "v" ["verbose"]
      (NoArg
        (\opt -> return opt { optVerbose = True }))
      "Enable verbose output"
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

  (token, mmUser) <- join (hoistE <$> mmLogin cd login)
  when (optVerbose opts) $ do
    putStrLn "Authenticated as:"
    pPrint mmUser
  let myId = getId mmUser

  mmWithWebSocket cd token printEvent checkForExit

printEvent :: WebsocketEvent -> IO ()
printEvent e = pPrint e

checkForExit :: MMWebSocket -> IO ()
checkForExit ws = do
  _ <- getLine
  return ()
