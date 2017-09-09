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

data Scheme = HTTP | HTTPS
            deriving (Eq, Show)

data ProxyType = Socks | Other
               deriving (Eq, Show)

newtype NormalizedEnv = NormalizedEnv [(String, String)]

proxyHostPermitted :: String -> IO Bool
proxyHostPermitted hostname = do
    result <- lookupEnv "NO_PROXY"
    case result of
        Nothing -> return True
        Just blacklist -> do
            let blacklistedHosts = splitOn "," blacklist
                allHostsBlocked = "*" `elem` blacklistedHosts
            return $ not $ (hostname `elem` blacklistedHosts) ||
                           allHostsBlocked

proxyForScheme :: Scheme -> IO (Maybe (ProxyType, String, Int))
proxyForScheme s = do
    env <- getEnvironment
    let proxy = case s of
          HTTP -> httpProxy
          HTTPS -> httpsProxy
    return $ proxy $ normalizeEnv env

httpProxy :: NormalizedEnv -> Maybe (ProxyType, String, Int)
httpProxy env = proxyFor "HTTP_PROXY" env <|>
                proxyFor "ALL_PROXY" env

httpsProxy :: NormalizedEnv -> Maybe (ProxyType, String, Int)
httpsProxy env = proxyFor "HTTPS_PROXY" env <|>
                 proxyFor "ALL_PROXY" env

proxyFor :: String -> NormalizedEnv -> Maybe (ProxyType, String, Int)
proxyFor name env = do
    val <- envLookup name env
    uri <- parseURI val

    let scheme = uriScheme uri
        getProxyType = isSocks <|> isHttp
        isSocks = if "socks" `isPrefixOf` scheme
                     then return Socks
                     else Nothing
        isHttp = if "https" `isPrefixOf` scheme ||
                    "http" `isPrefixOf` scheme
                    then return Other
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
