{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Aws.Lambda.Wai (waiHandler, waiHandler', WaiHandler) where

import           Aws.Lambda
import           Control.Concurrent.MVar
import           Data.Aeson
import qualified Data.Aeson              as Aeson
import qualified Data.Aeson.Types        as Aeson
import qualified Data.Binary.Builder     as Binary
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.CaseInsensitive    as CI
import qualified Data.HashMap.Strict     as HMap
import           Data.IORef
import qualified Data.IP                 as IP
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Text.Encoding      (decodeUtf8', encodeUtf8)
import qualified Data.Text.Encoding      as T
import qualified Data.Vault.Lazy         as Vault
import           GHC.IO.Unsafe           (unsafePerformIO)
import qualified Network.HTTP.Types      as H
import qualified Network.Socket          as Socket
import           Network.Wai             (Application)
import qualified Network.Wai             as Wai
import qualified Network.Wai.Internal    as Wai
import           Text.Read               (readMaybe)

type WaiHandler context = ApiGatewayRequest Text -> Context context -> IO (Either (ApiGatewayResponse Text) (ApiGatewayResponse Text))

waiHandler :: forall context. IO Wai.Application -> WaiHandler context
waiHandler initApp gatewayRequest context = initApp >>=
  \app -> waiHandler'' app gatewayRequest context

waiHandler' :: forall context. (context -> Wai.Application) -> WaiHandler context
waiHandler' getApp request context = do
  app <- getApp <$> readIORef (customContext context)
  waiHandler'' app request context

waiHandler'' :: forall context. Wai.Application -> WaiHandler context
waiHandler'' waiApplication gatewayRequest _ = do
  waiRequest <- mkWaiRequest gatewayRequest

  (status, headers, body) <- processRequest waiApplication waiRequest >>= readResponse

  print $ "Working: " <> ("Something went wai" :: ByteString)
  print $ "Actual response body (before decodeUtf8'): " <> body

  if BS.null body
  then return . pure . wrapInResponse (H.statusCode status) headers $ mempty
  else case decodeUtf8' body of
    Right responseBodyText -> do
        print $ "After decoding in wai: " <> responseBodyText
        return . pure . wrapInResponse (H.statusCode status) headers $ responseBodyText
    Left err -> error "Expected a response body that is valid UTF-8."

mkWaiRequest :: ApiGatewayRequest Text -> IO Wai.Request
mkWaiRequest ApiGatewayRequest{..} = do
  let ApiGatewayRequestContext{..} = apiGatewayRequestRequestContext
      ApiGatewayRequestContextIdentity{..} = apiGatewayRequestContextIdentity

  ip <- parseIp apiGatewayRequestContextIdentitySourceIp

  let pathInfo = H.decodePathSegments (encodeUtf8 apiGatewayRequestPath)

  let requestBodyRaw = maybe mempty T.encodeUtf8 apiGatewayRequestBody
  let requestBodyLength = Wai.KnownLength $ fromIntegral $ BS.length requestBodyRaw

  requestBodyMVar <- newMVar requestBodyRaw

  let requestBody = takeRequestBodyChunk requestBodyMVar
  let requestHeaderHost = encodeUtf8 <$> HMap.lookup "host" apiGatewayRequestHeaders
  let requestHeaderRange = encodeUtf8 <$> HMap.lookup "range" apiGatewayRequestHeaders
  let requestHeaderReferer = encodeUtf8 <$> HMap.lookup "referer" apiGatewayRequestHeaders
  let requestHeaderUserAgent = encodeUtf8 <$> HMap.lookup "User-Agent" apiGatewayRequestHeaders

  let queryParameters = toQueryStringParameters apiGatewayRequestQueryStringParameters
      rawQueryString = H.renderQuery True queryParameters
      httpVersion = getHttpVersion apiGatewayRequestContextProtocol

  let result = Wai.Request
                (encodeUtf8 apiGatewayRequestHttpMethod)
                httpVersion
                (encodeUtf8 apiGatewayRequestPath)
                rawQueryString
                (map toHeader $ HMap.toList apiGatewayRequestHeaders)
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

toQueryStringParameters :: Maybe [(Text, Maybe Text)] -> [H.QueryItem]
toQueryStringParameters (Just params@(p:ps)) =
  let toQueryItem (key, valueMay) = (encodeUtf8 key, encodeUtf8 <$> valueMay)
  in map toQueryItem params
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

wrapInResponse
  :: Int
  -> H.ResponseHeaders
  -> res
  -> ApiGatewayResponse res
wrapInResponse code responseHeaders response =
  ApiGatewayResponse code responseHeaders response False

toHeader :: (Text, Text) -> H.Header
toHeader (name, val) = (CI.mk . encodeUtf8 $ name, encodeUtf8 val)

tshow :: Show a => a -> Text
tshow = T.pack . show
