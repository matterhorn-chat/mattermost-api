{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Mattermost.Util where

import           Data.Char ( toUpper )
import qualified Data.Aeson as A

import           Control.Monad.Except
import           Control.Monad.State.Strict

import           Network.Mattermost.Types

-- A convenient, internal wrapper for IO actions that might fail.
newtype MM a = MM (ExceptT String (StateT ConnectionData IO) a)
  deriving (Functor, Applicative, Monad, MonadError String, MonadIO)

-- Lift an IO action into the MM monad
io :: IO a -> MM a
io = liftIO

-- Lift an IO action that produces an Either String into the MM monad
ioE :: IO (Either String a) -> MM a
ioE mote = do
  v <- io mote
  case v of
    Left  l -> throwError l
    Right r -> pure r

-- Lift an IO action that produces an Either s into the MM monad,
-- indicating what to do with the Left value to turn it into a
-- string
ioS :: (s -> String) -> IO (Either s a) -> MM a
ioS f mote = do
  v <- io mote
  case v of
    Left  l -> throwError (f l)
    Right r -> pure r

-- Lift a Maybe into the MM monad, with a supplied error message should
-- the value be Nothing
ioM :: String -> IO (Maybe a) -> MM a
ioM err mote = io mote >>= noteT err

runMM :: ConnectionData -> MM a -> IO (Either String a)
runMM cd (MM m) = fmap fst (runStateT (runExceptT m) cd)

noteT :: String -> Maybe r -> MM r
noteT l Nothing  = throwError l
noteT _ (Just r) = pure r

hoistT :: Either String r -> MM r
hoistT (Left l)  = throwError l
hoistT (Right r) = pure r

hoistA :: A.Result r -> MM r
hoistA (A.Error s)   = throwError s
hoistA (A.Success r) = pure r

assert :: String -> Bool -> MM ()
assert _ True  = pure ()
assert m False = throwError m

setToken :: Token -> MM ()
setToken b = MM $ modify $ \cd -> cd { cdToken = Just b }

getToken :: MM Token
getToken = MM $ do
  v <- cdToken `fmap` get
  case v of
    Nothing -> throwError "No token currently set!"
    Just x  -> pure x

getConnectionData :: MM ConnectionData
getConnectionData = MM get

-- | Case Insensitive string comparison
(~=) :: String -> String -> Bool
a ~= b = map toUpper a == map toUpper b
