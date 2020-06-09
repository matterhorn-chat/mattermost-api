{-# LANGUAGE ScopedTypeVariables #-}
module Network.Mattermost.Connection where


import           Control.Arrow (left)
import           Control.Exception (throwIO, IOException, try, throwIO)
import           Control.Monad (when)
import           Data.Maybe (isJust, listToMaybe)
import           Data.Monoid ((<>))
import           Data.Pool (destroyAllResources)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Char (toLower)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Network.HTTP.Base as HTTP
import qualified Network.HTTP.Headers as HTTP
import qualified Network.HTTP.Stream as HTTP
import qualified Network.HTTP.Media as HTTPM
import qualified Network.URI as URI
import           System.IO.Error (isEOFError)
import           Text.Read ( readMaybe )

import Network.Mattermost.Exceptions
import Network.Mattermost.Types
import Network.Mattermost.Types.Internal
import Network.Mattermost.Util

-- | Parse a path, failing if we cannot.
mmPath :: String -> IO URI.URI
mmPath str =
  noteE (URI.parseRelativeReference str)
        (URIParseException ("mmPath: " ++ str))

assertJSONResponse :: HTTP.Response_String -> IO ()
assertJSONResponse rsp = do
  contentType <- mmGetHeader rsp HTTP.HdrContentType

  let allowedTypes = [B.pack "application/json"]
  assertE (isJust $ HTTPM.matchContent allowedTypes $ B.pack contentType)
          (ContentTypeException
            ("Expected content type 'application/json';" ++
             " found " ++ contentType))

-- | Parse the JSON body out of a request, failing if it isn't an
--   'application/json' response, or if the parsing failed
jsonResponse :: A.FromJSON t => HTTP.Response_String -> IO t
jsonResponse rsp = do
  assertJSONResponse rsp

  hoistE $ left (\s -> JSONDecodeException s (HTTP.rspBody rsp))
                (A.eitherDecode (BL.pack (HTTP.rspBody rsp)))

-- | Parse the JSON body out of a request, failing if it isn't an
--   'application/json' response, or if the parsing failed
bytestringResponse :: HTTP.Response_String -> IO B.ByteString
bytestringResponse rsp =
  return (B.pack (HTTP.rspBody rsp))


noResponse :: HTTP.Response_String -> IO ()
noResponse _ = return ()


-- | Grab a header from the response, failing if it isn't present
mmGetHeader :: HTTP.Response_String -> HTTP.HeaderName -> IO String
mmGetHeader rsp hdr =
  noteE (HTTP.lookupHeader hdr (HTTP.rspHeaders rsp))
        (HeaderNotFoundException ("mmGetHeader: " ++ show hdr))

-- | Parse the JSON body out of a request, failing if it isn't an
--   'application/json' response, or if the parsing failed
mmGetJSONBody :: A.FromJSON t => String -> HTTP.Response_String -> IO (t)
mmGetJSONBody label rsp = do
  assertJSONResponse rsp

  let value = left (\s -> JSONDecodeException ("mmGetJSONBody: " ++ label ++ ": " ++ s)
                                              (HTTP.rspBody rsp))
                   (A.eitherDecode (BL.pack (HTTP.rspBody rsp)))
  hoistE $ do
    y <- value
    return (y)

doRequest :: Session
          -> HTTP.RequestMethod
          -> String
          -> B.ByteString
          -> IO HTTP.Response_String
doRequest (Session cd token) = submitRequest cd (Just token)

doUnauthRequest :: ConnectionData
                -> HTTP.RequestMethod
                -> String
                -> B.ByteString
                -> IO HTTP.Response_String
doUnauthRequest cd = submitRequest cd Nothing

-- | Submit an HTTP request.
--
-- If the request fails due to a 429 (rate-limited) response, this
-- raises 'RateLimitException' with the fields populated from the
-- response headers where possible.
--
-- If the response status is 2XX, the response is returned.
--
-- If the response status is anything else, its body is assumed to be
-- a JSON encoding of a Mattermost server error. If it can be decoded
-- as such, a 'MattermostError' exception is raised. Otherwise an
-- 'HTTPResponseException' is raised.
submitRequest :: ConnectionData
              -> Maybe Token
              -> HTTP.RequestMethod
              -> String
              -> B.ByteString
              -> IO HTTP.Response_String
submitRequest cd mToken method uri payload = do
  path <- buildPath cd (T.pack uri)
  parsedPath <- mmPath $ T.unpack path
  let contentLength = B.length payload
      authHeader =
          case mToken of
              Nothing -> []
              Just token -> [HTTP.mkHeader HTTP.HdrAuthorization ("Bearer " ++ getTokenString token)]

      request = HTTP.Request
        { HTTP.rqURI = parsedPath
        , HTTP.rqMethod = method
        , HTTP.rqHeaders =
          authHeader <>
          [ HTTP.mkHeader HTTP.HdrHost          (T.unpack $ cdHostname cd)
          , HTTP.mkHeader HTTP.HdrUserAgent     HTTP.defaultUserAgent
          , HTTP.mkHeader HTTP.HdrContentType   "application/json"
          , HTTP.mkHeader HTTP.HdrContentLength (show contentLength)
          ] ++ autoCloseToHeader (cdAutoClose cd)
        , HTTP.rqBody    = B.unpack payload
        }

      go = withConnection cd $ \con -> do
          runLogger cd "submitRequest" (HttpRequest method uri Nothing)
          result <- HTTP.simpleHTTP_ con request
          case result of
              Left e -> return $ Left e
              Right response -> do
                  when (shouldClose response) $ closeMMConn con
                  return $ Right response

  rawResponse <- do
      -- Try to submit the request. If we got an exception that we think
      -- indicates a network problem, we assume that to mean that the
      -- connection pool contained a connection that had been severed
      -- since it was last used. That means it's very likely that the
      -- pool has other stale connections in it, so we destroy all idle
      -- connections in the pool and try the request one more time. All
      -- other errors and exceptions are just propagated.
      resp :: Either IOException (Either HTTP.ConnError HTTP.Response_String)
           <- try go
      case resp of
          Left e | isConnectionError e -> do
              destroyAllResources (cdConnectionPool cd)
              go
          Left e -> throwIO e
          Right result -> return result

  rsp <- hoistE (left ConnectionException rawResponse)
  case HTTP.rspCode rsp of
    (4, 2, 9) -> do
        -- Extract rate limit information if possible
        let headers = HTTP.getHeaders rsp
            mLimit = readMaybe =<< findHeader rateLimitLimitHeader headers
            mRemaining = readMaybe =<< findHeader rateLimitRemainingHeader headers
            mReset = readMaybe =<< findHeader rateLimitResetHeader headers

        throwIO $ RateLimitException mLimit mRemaining mReset
    (2, _, _) -> return rsp
    code -> do
      case A.eitherDecode (BL.pack (HTTP.rspBody rsp)) of
        Right err ->
          throwIO (err :: MattermostError)
        Left _ ->
          throwIO (HTTPResponseException ("Server returned unexpected " ++ show code ++ " response"))

-- NOTE: At least as of HTTP-4000.3.14, custom header names are matched
-- case-sensitively when looking them up in responses. This is a bug
-- (reported at https://github.com/haskell/HTTP/issues/128) and in
-- the mean time we use our own header-matching implementation.
findHeader :: HTTP.HeaderName -> [HTTP.Header] -> Maybe String
findHeader n hs = HTTP.hdrValue <$> listToMaybe (filter (matchHeader n) hs)

matchHeader :: HTTP.HeaderName -> HTTP.Header -> Bool
matchHeader (HTTP.HdrCustom a) (HTTP.Header (HTTP.HdrCustom b) _) =
    (toLower <$> a) == (toLower <$> b)
matchHeader a (HTTP.Header b _) = a == b

rateLimitLimitHeader :: HTTP.HeaderName
rateLimitLimitHeader = HTTP.HdrCustom "X-RateLimit-Limit"

rateLimitRemainingHeader :: HTTP.HeaderName
rateLimitRemainingHeader = HTTP.HdrCustom "X-RateLimit-Remaining"

rateLimitResetHeader :: HTTP.HeaderName
rateLimitResetHeader = HTTP.HdrCustom "X-RateLimit-Reset"

isConnectionError :: IOException -> Bool
isConnectionError e =
    or [ isEOFError e
       -- There is not a specific predicate for "resource vanished"
       -- exceptions so "show" is as good as it gets.
       , "resource vanished" `List.isInfixOf` show e
       ]

shouldClose :: HTTP.Response_String -> Bool
shouldClose r =
    let isConnClose (HTTP.Header HTTP.HdrConnection v) = (toLower <$> v) == "close"
        isConnClose _ = False
    in any isConnClose $ HTTP.rspHeaders r

mkQueryString :: [Maybe (String, String)] -> String
mkQueryString ls =
  List.intercalate "&" [ URI.escapeURIString URI.isUnescapedInURIComponent k ++ "=" ++
                         URI.escapeURIString URI.isUnescapedInURIComponent v
                       | Just (k, v) <- ls ]

jsonBody :: A.ToJSON i => i -> B.ByteString
jsonBody = BL.toStrict . A.encode

noBody :: B.ByteString
noBody = B.empty


inPost
  :: String
  -> B.ByteString
  -> (HTTP.Response_String -> IO o)
  -> Session
  -> IO o
inPost uri payload k session =
  doRequest session HTTP.POST uri payload >>= k

inPut
  :: String
  -> B.ByteString
  -> (HTTP.Response_String -> IO o)
  -> Session
  -> IO o
inPut uri payload k session =
  doRequest session HTTP.PUT uri payload >>= k

inGet
  :: String
  -> B.ByteString
  -> (HTTP.Response_String -> IO o)
  -> Session
  -> IO o
inGet uri payload k session =
  doRequest session HTTP.GET uri payload >>= k

inDelete
  :: String
  -> B.ByteString
  -> (HTTP.Response_String -> IO o)
  -> Session
  -> IO o
inDelete uri payload k session =
  doRequest session HTTP.DELETE uri payload >>= k
