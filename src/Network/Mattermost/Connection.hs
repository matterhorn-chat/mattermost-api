module Network.Mattermost.Connection where


import           Control.Arrow (left)
import           Control.Exception (throwIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as List
import qualified Data.Text as T
import qualified Network.HTTP.Base as HTTP
import qualified Network.HTTP.Headers as HTTP
import qualified Network.HTTP.Stream as HTTP
import qualified Network.URI as URI

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

doRequest :: HTTP.RequestMethod -> String -> B.ByteString -> Session -> IO HTTP.Response_String
doRequest method uri payload (Session cd token) = do
  path <- mmPath ("/api/v4" ++ uri)
  rawResponse <- withConnection cd $ \con -> do
    let contentLength = B.length payload
        request = HTTP.Request
          { HTTP.rqURI = path
          , HTTP.rqMethod = method
          , HTTP.rqHeaders =
            [ HTTP.mkHeader HTTP.HdrAuthorization ("Bearer " ++ getTokenString token)
            , HTTP.mkHeader HTTP.HdrHost          (T.unpack $ cdHostname cd)
            , HTTP.mkHeader HTTP.HdrUserAgent     HTTP.defaultUserAgent
            , HTTP.mkHeader HTTP.HdrContentType   "application/json"
            , HTTP.mkHeader HTTP.HdrContentLength (show contentLength)
            ] ++ autoCloseToHeader (cdAutoClose cd)
          , HTTP.rqBody    = B.unpack payload
          }
    HTTP.simpleHTTP_ con request
  rsp <- hoistE (left ConnectionException rawResponse)
  case HTTP.rspCode rsp of
    (2, _, _) -> return rsp
    code -> do
      case A.eitherDecode (BL.pack (HTTP.rspBody rsp)) of
        Right err ->
          throwIO (err :: MattermostError)
        Left _ ->
          throwIO (HTTPResponseException ("Server returned unexpected " ++ show code ++ " response"))


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
  doRequest HTTP.POST uri payload session >>= k

inPut
  :: String
  -> B.ByteString
  -> (HTTP.Response_String -> IO o)
  -> Session
  -> IO o
inPut uri payload k session =
  doRequest HTTP.PUT uri payload session >>= k

inGet
  :: String
  -> B.ByteString
  -> (HTTP.Response_String -> IO o)
  -> Session
  -> IO o
inGet uri payload k session =
  doRequest HTTP.GET uri payload session >>= k

inDelete
  :: String
  -> B.ByteString
  -> (HTTP.Response_String -> IO o)
  -> Session
  -> IO o
inDelete uri payload k session =
  doRequest HTTP.DELETE uri payload session >>= k



doUnauthRequest :: HTTP.RequestMethod -> String -> B.ByteString -> ConnectionData -> IO HTTP.Response_String
doUnauthRequest method uri payload cd = do
  path <- mmPath ("/api/v4" ++ uri)
  rawResponse <- withConnection cd $ \con -> do
    let contentLength = B.length payload
        request = HTTP.Request
          { HTTP.rqURI = path
          , HTTP.rqMethod = method
          , HTTP.rqHeaders =
            [ HTTP.mkHeader HTTP.HdrHost          (T.unpack $ cdHostname cd)
            , HTTP.mkHeader HTTP.HdrUserAgent     HTTP.defaultUserAgent
            , HTTP.mkHeader HTTP.HdrContentType   "application/json"
            , HTTP.mkHeader HTTP.HdrContentLength (show contentLength)
            ] ++ autoCloseToHeader (cdAutoClose cd)
          , HTTP.rqBody    = B.unpack payload
          }
    HTTP.simpleHTTP_ con request
  rsp <- hoistE (left ConnectionException rawResponse)
  case HTTP.rspCode rsp of
    (2, _, _) -> return rsp
    code -> throwIO (HTTPResponseException ("Server returned unexpected " ++ show code ++ " response"))
