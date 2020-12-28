{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Aws.Lambda.Wai
  ( runWaiAsLambda,
    apiGatewayWaiHandler,
    ApiGatewayWaiHandler,
    albWaiHandler,
    ALBWaiHandler,
    ignoreALBPathPart,
    ignoreNothing,
  )
where

import Aws.Lambda
import Aws.Lambda.Setup
import Control.Concurrent.MVar
import Control.Exception (throwIO)
import qualified Data.Binary.Builder as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HMap
import Data.IORef
import qualified Data.IP as IP
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import qualified Data.Text.Encoding as T
import qualified Data.Vault.Lazy as Vault
import qualified Network.HTTP.Types as H
import qualified Network.Socket as Socket
import Network.Wai (Application)
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import System.Environment (lookupEnv)
import qualified System.IO as IO
import Text.Read (readMaybe)

type ApiGatewayWaiHandler = ApiGatewayRequest Text -> Context Application -> IO (Either (ApiGatewayResponse Text) (ApiGatewayResponse Text))

type ALBWaiHandler = ALBRequest Text -> Context Application -> IO (Either (ALBResponse Text) (ALBResponse Text))

newtype ALBIgnoredPathPortion = ALBIgnoredPathPortion {unALBIgnoredPathPortion :: Text}

runWaiAsLambda :: DispatcherOptions -> HandlerName -> Maybe ALBIgnoredPathPortion -> IO Application -> IO ()
runWaiAsLambda options handlerName ignoredPath mkApp = do
  let proxyTypeEnvVar = "ENV_PROXY_TYPE"
  proxyTypeMay <- lookupEnv proxyTypeEnvVar

  case proxyTypeMay of
    Just proxyType ->
      case proxyType of
        "apigw" -> do
          IO.print $ "Starting Lambda using API gateway handler '" <> unHandlerName handlerName <> "'."
          runLambdaHaskellRuntime options mkApp id $ do
            addAPIGatewayHandler handlerName apiGatewayWaiHandler
        "alb" -> do
          IO.print $ "Starting Lambda using ALB handler '" <> unHandlerName handlerName <> "'."
          runLambdaHaskellRuntime options mkApp id $ do
            addALBHandler handlerName (albWaiHandler ignoredPath)
        other ->
          throwIO $
            userError $
              "'" <> other <> "' is not a valid value for " <> proxyTypeEnvVar <> ". Supported values are 'apigw' and 'alb' (excluding ticks)."
    Nothing ->
      throwIO $
        userError $
          "Could not determine the proxy type to use for your Lambda function."
            <> " Please set the "
            <> proxyTypeEnvVar
            <> " environment variable to 'apigw' or 'alb' (excluding ticks) to use API Gateway or ALB respectively."

ignoreALBPathPart :: Text -> Maybe ALBIgnoredPathPortion
ignoreALBPathPart = Just . ALBIgnoredPathPortion

ignoreNothing :: Maybe ALBIgnoredPathPortion
ignoreNothing = Nothing

albWaiHandler :: Maybe ALBIgnoredPathPortion -> ALBWaiHandler
albWaiHandler ignoredPathPortion request context = do
  waiApplication <- readIORef (customContext context)
  waiRequest <- mkWaiRequestFromALB ignoredPathPortion request

  (status, headers, body) <- processRequest waiApplication waiRequest >>= readResponse

  if BS.null body
    then return . pure . mkALBResponse (H.statusCode status) headers $ mempty
    else case decodeUtf8' body of
      Right responseBodyText ->
        return . pure . mkALBResponse (H.statusCode status) headers $ responseBodyText
      Left err -> error $ "Expected a response body that is valid UTF-8: " <> show err

apiGatewayWaiHandler :: ApiGatewayWaiHandler
apiGatewayWaiHandler request context = do
  waiApplication <- readIORef (customContext context)
  waiRequest <- mkWaiRequestFromApiGw request

  (status, headers, body) <- processRequest waiApplication waiRequest >>= readResponse

  if BS.null body
    then return . pure . mkApiGatewayResponse (H.statusCode status) headers $ mempty
    else case decodeUtf8' body of
      Right responseBodyText ->
        return . pure . mkApiGatewayResponse (H.statusCode status) headers $ responseBodyText
      Left err -> error $ "Expected a response body that is valid UTF-8: " <> show err

mkWaiRequestFromALB :: Maybe ALBIgnoredPathPortion -> ALBRequest Text -> IO Wai.Request
mkWaiRequestFromALB (fmap unALBIgnoredPathPortion -> pathPortionToIgnore) ALBRequest {..} = do
  let sourceIpMay = albRequestHeaders >>= HMap.lookup "x-forwarded-for"

  ip <- parseIp sourceIpMay

  let requestPath =
        case pathPortionToIgnore of
          Just toIgnore ->
            let toIgnoreSafe = "/" <> T.dropWhile (\c -> c == '/' || c == '\\') toIgnore
                throwPathError =
                  error $
                    "Given path piece to ignore '"
                      <> T.unpack toIgnoreSafe
                      <> "' is longer than the received request path "
                      <> T.unpack albRequestPath
                      <> "!"
             in fromMaybe throwPathError $ T.stripPrefix toIgnoreSafe albRequestPath
          Nothing -> albRequestPath

  -- TODO: Duplication
  let pathInfo = H.decodePathSegments (encodeUtf8 requestPath)

  let requestBodyRaw = maybe mempty T.encodeUtf8 albRequestBody
  let requestBodyLength = Wai.KnownLength $ fromIntegral $ BS.length requestBodyRaw

  requestBodyMVar <- newMVar requestBodyRaw

  let requestBody = takeRequestBodyChunk requestBodyMVar
  let headers = fromMaybe HMap.empty albRequestHeaders
  let requestHeaderHost = encodeUtf8 <$> HMap.lookup "host" headers
  let requestHeaderRange = encodeUtf8 <$> HMap.lookup "range" headers
  let requestHeaderReferer = encodeUtf8 <$> HMap.lookup "referer" headers
  let requestHeaderUserAgent = encodeUtf8 <$> HMap.lookup "User-Agent" headers

  let queryParameters = toQueryStringParameters albRequestQueryStringParameters
      rawQueryString = H.renderQuery True queryParameters
      httpVersion = H.http11 -- ALB converts even HTTP/2 requests to 1.1
  let result =
        Wai.Request
          (encodeUtf8 albRequestHttpMethod)
          httpVersion
          (encodeUtf8 requestPath)
          rawQueryString
          (map toHeader $ HMap.toList headers)
          True -- We assume it's always secure as we're passing through API Gateway
          ip
          pathInfo
          queryParameters
          requestBody
          Vault.empty
          requestBodyLength
          requestHeaderHost
          requestHeaderRange
          requestHeaderReferer
          requestHeaderUserAgent

  return result

mkWaiRequestFromApiGw :: ApiGatewayRequest Text -> IO Wai.Request
mkWaiRequestFromApiGw ApiGatewayRequest {..} = do
  let ApiGatewayRequestContext {..} = apiGatewayRequestRequestContext
      ApiGatewayRequestContextIdentity {..} = apiGatewayRequestContextIdentity

  ip <- parseIp apiGatewayRequestContextIdentitySourceIp

  let requestPath =
        -- We prefer the proxied path because apiGatewayRequestPath also
        -- includes the resource which we don't need
        case apiGatewayRequestPathParameters of
          Just pathParametersMap ->
            case HMap.lookup "proxy" pathParametersMap of
              Just proxyPath -> proxyPath
              Nothing -> apiGatewayRequestPath
          Nothing -> apiGatewayRequestPath

  let pathInfo = H.decodePathSegments (encodeUtf8 requestPath)

  let requestBodyRaw = maybe mempty T.encodeUtf8 apiGatewayRequestBody
  let requestBodyLength = Wai.KnownLength $ fromIntegral $ BS.length requestBodyRaw

  requestBodyMVar <- newMVar requestBodyRaw

  let requestBody = takeRequestBodyChunk requestBodyMVar
  let headers = fromMaybe HMap.empty apiGatewayRequestHeaders
  let requestHeaderHost = encodeUtf8 <$> HMap.lookup "host" headers
  let requestHeaderRange = encodeUtf8 <$> HMap.lookup "range" headers
  let requestHeaderReferer = encodeUtf8 <$> HMap.lookup "referer" headers
  let requestHeaderUserAgent = encodeUtf8 <$> HMap.lookup "User-Agent" headers

  let queryParameters = toQueryStringParameters apiGatewayRequestQueryStringParameters
      rawQueryString = H.renderQuery True queryParameters
      httpVersion = getHttpVersion apiGatewayRequestContextProtocol

  let result =
        Wai.Request
          (encodeUtf8 apiGatewayRequestHttpMethod)
          httpVersion
          (encodeUtf8 requestPath)
          rawQueryString
          (map toHeader $ HMap.toList headers)
          True -- We assume it's always secure as we're passing through API Gateway
          ip
          pathInfo
          queryParameters
          requestBody
          Vault.empty
          requestBodyLength
          requestHeaderHost
          requestHeaderRange
          requestHeaderReferer
          requestHeaderUserAgent

  return result

getHttpVersion :: Text -> H.HttpVersion
getHttpVersion protocol
  | "0.9" `T.isSuffixOf` protocol = H.http09
  | "1.0" `T.isSuffixOf` protocol = H.http10
  | "1.1" `T.isSuffixOf` protocol = H.http11
  | "2.0" `T.isSuffixOf` protocol = H.http20
  | otherwise = H.http11

takeRequestBodyChunk :: MVar ByteString -> IO ByteString
takeRequestBodyChunk requestBodyMVar = do
  result <- tryTakeMVar requestBodyMVar
  case result of
    Just bs -> pure bs
    Nothing -> pure BS.empty

toQueryStringParameters :: Maybe (HMap.HashMap Text Text) -> [H.QueryItem]
toQueryStringParameters (Just params) =
  let toQueryItem (key, value) = (encodeUtf8 key, Just $ encodeUtf8 value)
   in map toQueryItem $ HMap.toList params
toQueryStringParameters _ = []

parseIp :: Maybe Text -> IO Socket.SockAddr
parseIp sourceIpText =
  case sourceIpText of
    Just sourceIp ->
      case readMaybe (T.unpack sourceIp) of
        Just ip ->
          pure $ case ip of
            IP.IPv4 ip4 ->
              Socket.SockAddrInet
                0 -- default port
                (IP.toHostAddress ip4)
            IP.IPv6 ip6 ->
              Socket.SockAddrInet6
                0 -- default port
                0 -- flow info
                (IP.toHostAddress6 ip6)
                0 -- scope id
        Nothing -> error "Could not parse source ip."
    Nothing -> error "Missing source ip."

processRequest :: Application -> Wai.Request -> IO Wai.Response
processRequest app req = do
  mvar <- newEmptyMVar
  Wai.ResponseReceived <- app req $ \resp -> do
    putMVar mvar resp
    pure Wai.ResponseReceived
  takeMVar mvar

readResponse :: Wai.Response -> IO (H.Status, H.ResponseHeaders, ByteString)
readResponse (Wai.responseToStream -> (st, hdrs, mkBody)) = do
  body <- mkBody drainBody
  pure (st, hdrs, body)
  where
    drainBody :: Wai.StreamingBody -> IO ByteString
    drainBody body = do
      ioRef <- newIORef Binary.empty
      body
        (\b -> atomicModifyIORef ioRef (\b' -> (b <> b', ())))
        (pure ())
      BL.toStrict . Binary.toLazyByteString <$> readIORef ioRef

toHeader :: (Text, Text) -> H.Header
toHeader (name, val) = (CI.mk . encodeUtf8 $ name, encodeUtf8 val)