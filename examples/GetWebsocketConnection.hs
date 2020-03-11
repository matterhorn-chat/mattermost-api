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
  cd <- initConnectionData (configHostname config) (fromIntegral (configPort config))
                           (ConnectHTTPS True) defaultConnectionPoolConfig

  let login = Login { username = configUsername config
                    , password = configPassword config
                    }

  (session, mmUser) <- join (hoistE <$> mmLogin cd login)
  when (optVerbose opts) $ do
    putStrLn "Authenticated as:"
    pPrint mmUser

  mmWithWebSocket session printEvent checkForExit

printEvent :: Either String (Either WebsocketActionResponse WebsocketEvent) -> IO ()
printEvent we = pPrint we

checkForExit :: MMWebSocket -> IO ()
checkForExit ws = do
  ln <- getLine
  if ln == "exit"
    then mmCloseWebSocket ws
    else checkForExit ws
