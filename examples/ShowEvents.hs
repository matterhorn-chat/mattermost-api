module Main(main) where

import           Control.Monad ( when, join )
import           Data.Bits (xor)
import           Data.Char (ord)
import           Data.Word (Word8)
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

  mmWithWebSocket cd token (printEvent cd token) checkForExit

hash :: String -> Int
hash = foldl xor 0 . fmap ord

color :: String -> String
color s = h ++ s ++ "\x1b[39m"
  where h = case fromIntegral (hash s) `mod` 13 of
          1  -> "\x1b[32m"
          2  -> "\x1b[33m"
          3  -> "\x1b[34m"
          4  -> "\x1b[35m"
          5  -> "\x1b[36m"
          6  -> "\x1b[37m"
          7  -> "\x1b[91m"
          8  -> "\x1b[92m"
          9  -> "\x1b[93m"
          10 -> "\x1b[94m"
          11 -> "\x1b[95m"
          12 -> "\x1b[96m"
          _  -> "\x1b[31m"

printEvent :: ConnectionData -> Token -> WebsocketEvent -> IO ()
printEvent cd token we = do
  let tId = weTeamId we
      cId = weChannelId we
  profiles <- mmGetProfiles cd token tId
  channel <- mmGetChannel cd token tId cId
  let cName = color ("#" ++ channelName channel)
  case weAction we of
    WMPosted -> case wepPost (weProps we) of
      Just (Post { postMessage = msg
                 , postUserId  = usrId
                 }) -> do
        let nick = color ("@" ++ userProfileUsername (profiles HM.! usrId))
        putStrLn (nick ++ " in " ++ cName ++ ":  " ++ msg)
      Nothing -> return ()
    WMPostEdited -> case wepPost (weProps we) of
      Just (Post { postMessage = msg
                 , postUserId  = usrId
                 }) -> do
        let nick = color ("@" ++ userProfileUsername (profiles HM.! usrId))
        putStrLn (nick ++ " [edit]:  " ++ msg)
      Nothing -> return ()
    WMPostDeleted -> case wepPost (weProps we) of
      Just (Post { postMessage = msg
                 , postUserId  = usrId
                 }) -> do
        let nick = color ("@" ++ userProfileUsername (profiles HM.! usrId))
        putStrLn (nick ++ " [deletion]:  " ++ msg)
      Nothing -> return ()
    _ -> return ()

checkForExit :: MMWebSocket -> IO ()
checkForExit ws = do
  putStrLn "Connected. Press enter to quit."
  ln <- getLine
  mmCloseWebSocket ws
