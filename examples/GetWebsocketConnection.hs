module Main(main) where

import           Control.Monad ( when, join )
import qualified Data.Text as T
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

import           Config
import           LocalConfig -- You will need to define a function:
                             -- getConfig :: IO Config
                             -- See Config.hs

data Options
  = Options
  { optChannel :: String
  , optVerbose :: Bool
  , optOffset  :: Int
  , optLimit   :: Int
  } deriving (Read, Show)

defaultOptions :: Options
defaultOptions = Options
  { optChannel = "town-square"
  , optVerbose = False
  , optOffset  = 0
  , optLimit   = 10
  }

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "c" ["channel"]
      (ReqArg
        (\arg opt -> return opt { optChannel = arg })
        "CHANNEL")
      "Channel to fetch posts from"
  , Option "v" ["verbose"]
      (NoArg
        (\opt -> return opt { optVerbose = True }))
      "Enable verbose output"
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
  let cd      = mkConnectionData (T.unpack (configHostname config))
                                 (fromIntegral (configPort config))
                                 ctx
      login   = Login { username = configUsername config
                      , password = configPassword config
                      , teamname = configTeam     config }

  (token, mmUser) <- join (hoistE <$> mmLogin cd login)
  when (optVerbose opts) $ do
    putStrLn "Authenticated as:"
    pPrint mmUser

  mmWsConnect cd token
