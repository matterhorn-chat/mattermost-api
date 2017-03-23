{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import           Control.Exception as E
import           Control.Concurrent ( myThreadId )
import           System.Posix.Signals ( installHandler
                                      , keyboardSignal
                                      , Handler(..)
                                      )
import           Control.Monad ( when, join, forever )
import           Control.Monad.IO.Class ( liftIO )
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
import           System.Console.Haskeline ( InputT
                                          , defaultSettings
                                          , getInputLine
                                          , withInterrupt
                                          , handleInterrupt
                                          , simpleCompletion
                                          , completeWord
                                          , setComplete
                                          , Completion
                                          , Settings(..)
                                          , runInputT )
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
  tid  <- myThreadId
  installHandler keyboardSignal (Catch (throwTo tid UserInterrupt)) Nothing
  args <- getArgs
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions

  config <- getConfig -- see LocalConfig import
  let repl = do
        ctx    <- initConnectionContext
        let cd      = mkConnectionData (T.unpack (configHostname config))
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

        i <- mmGetInitialLoad cd token
        let [myTeam] = [ t | t <- initialLoadTeams i
                           , teamName t == T.unpack (configTeam config)
                           ]
        channels <- mmGetChannels cd token (getId myTeam)
        users    <- mmGetProfiles cd token (getId myTeam)

        let channelNameMap = HM.fromList [ (channelName c, c)
                                         | c <- channels
                                         ]
            channelIdMap   = HM.fromList [ (getId c, c)
                                         | c <- channels
                                         ]
            userMap        = HM.fromList [ (userUsername u, u)
                                         | u <- HM.elems users
                                         ]

        mmWithWebSocket
          cd
          token
          (printEvent cd token users channelIdMap)
          (checkForExit cd token myId (getId myTeam) channelNameMap userMap)
  let loop = repl `catch` (\UserInterrupt -> loop)
                  `catch` (\(SomeException e) ->  pPrint e >> loop)
  loop

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

printEvent :: ConnectionData
           -> Token
           -> HM.HashMap UserId    UserProfile
           -> HM.HashMap ChannelId Channel
           -> WebsocketEvent -> IO ()
printEvent cd token profiles chanMap we = do
  let tId = weTeamId we
      cId = weChannelId we
      channel = chanMap HM.! cId
  case weEvent we of
    WMPosted -> case wepPost (weData we) of
      Just (Post { postMessage = msg
                 , postUserId  = usrId
                 }) -> do
        let nick  = color ("@" ++ userUsername (profiles HM.! usrId))
            cName = color ("#" ++ channelName channel)
        if channelType channel == "O"
          then putStrLn (nick ++ " in " ++ cName ++ ":  " ++ msg)
          else putStrLn ("dm by " ++ nick ++ ":  " ++ msg)
      Nothing -> return ()
    WMPostEdited -> case wepPost (weData we) of
      Just (Post { postMessage = msg
                 , postUserId  = usrId
                 }) -> do
        let nick = color ("@" ++ userUsername (profiles HM.! usrId))
        putStrLn (nick ++ " [edit]:  " ++ msg)
      Nothing -> return ()
    WMPostDeleted -> case wepPost (weData we) of
      Just (Post { postMessage = msg
                 , postUserId  = usrId
                 }) -> do
        let nick = color ("@" ++ userUsername (profiles HM.! usrId))
        putStrLn (nick ++ " [deletion]:  " ++ msg)
      Nothing -> return ()
    _ -> return ()

data Focus = NoFocus
           | ChannelFocus String
           | DMFocus      String -- ^ Channel Name
                          String -- ^ Username

isChannelOrDM :: Focus -> Bool
isChannelOrDM NoFocus = False
isChannelOrDM _       = True

checkForExit :: ConnectionData
             -> Token
             -> UserId
             -> TeamId
             -> HM.HashMap String Channel
             -> HM.HashMap String UserProfile
             -> MMWebSocket
             -> IO ()
checkForExit cd token userId teamId channelMap userMap ws = do
  let usernameList = map ("@"++) (HM.keys userMap)
                     ++ HM.keys userMap
      channelList  = map ("#"++) (HM.keys channelMap)
                     ++ HM.keys channelMap
      commandList  = ["/unfocus"
                     ,"/focus"
                     ,"/direct"
                     ,"/channels"
                     ,"/users"
                     ,"/help"
                     ,"/quit"
                     ]
      wordList     = usernameList ++ channelList ++ commandList
      searchFunc s = return (map simpleCompletion (filter (s `isPrefixOf`) wordList))
      settings     = setComplete (completeWord Nothing " \t" searchFunc) defaultSettings
  runInputT settings (getCommand NoFocus)
  where
        getCommand :: Focus -> InputT IO ()
        getCommand focus = do
          let prompt = case focus of
                       NoFocus          -> "mm> "
                       ChannelFocus f   -> "#" ++ f ++ "> "
                       DMFocus      _ u -> "@" ++ u ++ "> "
          ln <- getInputLine prompt
          case ln of
            Nothing       -> return ()
            Just ('/':rs) -> runCommand (words rs) focus
            Just ln       -> putMessage ln focus
        runCommand :: [String] -> Focus -> InputT IO ()
        runCommand ["unfocus"] old = getCommand NoFocus
        runCommand ["focus", room] old
          | HM.member room channelMap = do
              liftIO $ putStrLn (" + setting focus to #" ++ room)
              getCommand (ChannelFocus room)
          | otherwise = do
              liftIO $ putStrLn ("I don't know the channel #" ++ room)
              getCommand old
        runCommand ["direct", user] old
          | HM.member user userMap = do
              liftIO $ putStrLn (" + setting focus to @" ++ user)
              let [loUser, hiUser] = sort [ getId (userMap HM.! user), userId ]
              let cname = idString loUser ++ "__" ++ idString hiUser
              getCommand (DMFocus cname user)
          | otherwise = do
              liftIO $ putStrLn ("I don't know the user @" ++ user)
              getCommand old
        runCommand ["quit"] _ = do
          liftIO $ putStrLn "Quitting"
          liftIO $ mmCloseWebSocket ws
        runCommand ["channels"] focus = do
          liftIO $ putStrLn "Available channels include:"
          liftIO $ sequence_ [ putStrLn ("  #" ++ channelName c)
                             | c <- HM.elems channelMap
                             , channelType c == "O"
                             ]
          getCommand focus
        runCommand ["users"] focus = do
          liftIO $ putStrLn "Available users include:"
          liftIO $ sequence_ [ putStrLn ("  @" ++ u)
                             | u <- HM.keys userMap
                             ]
          getCommand focus
        runCommand ["help"] focus = do
          liftIO $ putStrLn "Available commands:"
          liftIO $ putStrLn "  /unfocus"
          liftIO $ putStrLn "  /focus [room]"
          liftIO $ putStrLn "  /direct [username]"
          liftIO $ putStrLn "  /channels"
          liftIO $ putStrLn "  /users"
          liftIO $ putStrLn "  /help"
          liftIO $ putStrLn "  /quit"
          getCommand focus
        runCommand cmd focus = do
          liftIO $ putStrLn ("Unknown command: " ++ unwords cmd)
          getCommand focus
        putMessage :: String -> Focus -> InputT IO ()
        putMessage [] focus   = do
          getCommand focus
        putMessage ln NoFocus = do
          liftIO $ putStrLn "I don't know where to send that message."
          liftIO $ putStrLn "Set your target with /focus [channel-name] or /direct [username]"
          getCommand NoFocus
        putMessage ln focus = do
          let rm = case focus of
                   NoFocus           -> error "the impossible"
                   ChannelFocus rm   -> rm
                   DMFocus      rm _ -> rm
          let c = channelMap HM.! rm
          pendingPost <- liftIO $ mkPendingPost ln userId (getId c)
          post <- liftIO $ mmPost cd token teamId pendingPost
          getCommand focus
