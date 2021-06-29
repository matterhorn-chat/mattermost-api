module Network.Mattermost.Proxy
  ( Scheme(..)
  , ProxyType(..)
  , proxyForScheme
  , proxyHostPermitted
  )
where

import Control.Applicative ((<|>))
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Network.URI (parseURI, uriRegName, uriPort, uriAuthority, uriScheme)
import System.Environment (getEnvironment, lookupEnv)
import Text.Read (readMaybe)

data Scheme = HTTPS
            deriving (Eq, Show)

data ProxyType = Socks
               deriving (Eq, Show)

newtype NormalizedEnv = NormalizedEnv [(String, String)]

proxyHostPermitted :: String -> IO Bool
proxyHostPermitted hostname = do
    result <- lookupEnv "NO_PROXY"
    case result of
        Nothing -> return True
        Just blacklist -> do
            let patterns = splitOn "," blacklist
                hostnameParts = reverse $ splitOn "." hostname
                isBlacklisted = any matches patterns
                matches pat =
                    let patParts = reverse $ splitOn "." pat
                        go [] [] = True
                        go [] _ = False
                        go _ [] = False
                        go (p:pParts) (h:hTail) =
                            if p == "*"
                            then True
                            else p == h && go pParts hTail
                    in go patParts hostnameParts
            return $ not isBlacklisted

proxyForScheme :: Scheme -> IO (Maybe (ProxyType, String, Int))
proxyForScheme s = do
    env <- getEnvironment
    let proxy = case s of
          HTTPS -> httpsProxy
    return $ proxy $ normalizeEnv env

httpsProxy :: NormalizedEnv -> Maybe (ProxyType, String, Int)
httpsProxy env = proxyFor "HTTPS_PROXY" env <|>
                 proxyFor "ALL_PROXY" env

proxyFor :: String -> NormalizedEnv -> Maybe (ProxyType, String, Int)
proxyFor name env = do
    val <- envLookup name env
    uri <- parseURI val

    let scheme = uriScheme uri
        getProxyType = isSocks
        isSocks = if "socks" `isPrefixOf` scheme
                     then return Socks
                     else Nothing

    ty <- getProxyType
    auth <- uriAuthority uri
    port <- readMaybe (drop 1 $ uriPort auth)
    return (ty, uriRegName auth, port)

normalizeEnv :: [(String, String)] -> NormalizedEnv
normalizeEnv env =
    let norm (k, v) = (normalizeVar k, v)
    in NormalizedEnv $ norm <$> env

normalizeVar :: String -> String
normalizeVar = (toLower <$>)

envLookup :: String -> NormalizedEnv -> Maybe String
envLookup v (NormalizedEnv env) = lookup (normalizeVar v) env
