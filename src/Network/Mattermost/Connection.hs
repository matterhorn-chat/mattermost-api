{-# LANGUAGE ScopedTypeVariables #-}
module Network.Mattermost.Connection where


import           Control.Arrow (left)
import           Control.Exception (throwIO, IOException, try, throwIO)
import           Control.Monad (when)
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
import qualified Network.URI as URI
import           System.IO.Error (isEOFError)

import Network.Mattermost.Exceptions
import Network.Mattermost.Types
import Network.Mattermost.Types.Internal
import Network.Mattermost.Util

-- | Parse a path, failing if we cannot.
mmPath :: String -> IO URI.URI
mmPath str =
  noteE (URI.parseRelativeReference str)
        (URIParseException ("mmPath: " ++ str))


-- | Parse the JSON body out of a request, failing if it isn't an
--   'application/json' response, or if the parsing failed
jsonResponse :: A.FromJSON t => HTTP.Response_String -> IO t
jsonResponse rsp = do
  contentType <- mmGetHeader rsp HTTP.HdrContentType
  assertE (contentType ~= "application/json")
          (ContentTypeException
            ("Expected content type 'application/json'" ++
             " found " ++ contentType))

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
  contentType <- mmGetHeader rsp HTTP.HdrContentType
  assertE (contentType ~= "application/json")
          (ContentTypeException
            ("mmGetJSONBody: " ++ label ++ ": " ++
             "Expected content type 'application/json'" ++
             " found " ++ contentType))

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

submitRequest :: ConnectionData
              -> Maybe Token
              -> HTTP.RequestMethod
              -> String
              -> B.ByteString
              -> IO HTTP.Response_String
submitRequest cd mToken method uri payload = do
  path <- mmPath ("/api/v4" ++ uri)
  let contentLength = B.length payload
      authHeader =
          case mToken of
              Nothing -> []
              Just token -> [HTTP.mkHeader HTTP.HdrAuthorization ("Bearer " ++ getTokenString token)]

      request = HTTP.Request
        { HTTP.rqURI = path
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
      -- Try to submit the request. If we got an EOF exception, that
      -- means that the connection pool contained a connection that
      -- had been severed since it was last used. That means it's
      -- very likely that the pool has other stale connections in it,
      -- so we destroy all idle connections in the pool and try the
      -- request one more time. All other errors and exceptions are just
      -- propagated.
      resp :: Either IOException (Either HTTP.ConnError HTTP.Response_String)
           <- try go
      case resp of
          Left e | isEOFError e -> do
              destroyAllResources (cdConnectionPool cd)
              go
          Left e -> throwIO e
          Right result -> return result

  rsp <- hoistE (left ConnectionException rawResponse)
  case HTTP.rspCode rsp of
    (2, _, _) -> return rsp
    code -> do
      case A.eitherDecode (BL.pack (HTTP.rspBody rsp)) of
        Right err ->
          throwIO (err :: MattermostError)
        Left _ ->
          throwIO (HTTPResponseException ("Server returned unexpected " ++ show code ++ " response"))

shouldClose :: HTTP.Response_String -> Bool
shouldClose r =
    let isConnClose (HTTP.Header HTTP.HdrConnection v) = (toLower <$> v) == "close"
        isConnClose _ = False
    in any isConnClose $ HTTP.rspHeaders r

mkQueryString :: [Maybe (String, String)] -> String
mkQueryString ls =
  List.intercalate "&" [ k ++ "=" ++ v | Just (k, v) <- ls ]

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
