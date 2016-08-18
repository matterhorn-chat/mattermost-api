module Main(main) where

import           Control.Monad ( when, join )
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

  let getUser = mmGetUser cd token
  mmWithWebSocket cd token (printEvent getUser) checkForExit

printEvent :: (UserId -> IO User) -> WebsocketEvent -> IO ()
printEvent getUser we =
  case weAction we of
    WMPosted -> case wepPost (weProps we) of
      Just (Post { postMessage = msg
                 , postUserId  = usrId
                 }) -> do
        user <- getUser usrId
        let nick = userUsername user
        putStrLn (nick ++ ": " ++ msg)
      Nothing -> return ()
    WMPostEdited -> case wepPost (weProps we) of
      Just (Post { postMessage = msg
                 , postUserId  = usrId
                 }) -> do
        user <- getUser usrId
        let nick = userUsername user
        putStrLn (nick ++ " [edited previous post to]: " ++ msg)
      Nothing -> return ()
    WMPostDeleted -> case wepPost (weProps we) of
      Just (Post { postMessage = msg
                 , postUserId  = usrId
                 }) -> do
        user <- getUser usrId
        let nick = userUsername user
        putStrLn (nick ++ " [deleted previous post which read]: " ++ msg)
      Nothing -> return ()
    _ -> return ()

checkForExit :: MMWebSocket -> IO ()
checkForExit ws = do
  putStrLn "Connected. Press enter to quit."
  ln <- getLine
  mmCloseWebSocket ws
