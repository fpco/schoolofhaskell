-- | Handles communicating with the server, to initialize the SoH
-- container.
module ContainerClient
  ( BaseUrl(..)
  , listContainers
  , createContainer
  , getContainerDetailById
  , getContainerDetailByReceipt
  , stopContainerById
  , stopContainerByReceipt
  , pollForContainerAddress
  , mschedulerUrl
  , AjaxException(..)
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Exception (throwIO, Exception)
import           Control.Lens
import qualified Data.Aeson as Aeson
import           Data.ByteString.Lazy (toStrict)
import           Data.Text (pack)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Typeable (Typeable)
import qualified Data.UUID.Types as UUID
import           Import
import qualified JavaScript.JQuery as JQ
import qualified JavaScript.JQuery.Internal as JQ
import           SchoolOfHaskell.Scheduler.API

newtype BaseUrl = BaseUrl Text

listContainers :: BaseUrl -> IO [ContainerId]
listContainers bu =
  sendRequestJsonResponse bu "containers" "" JQ.GET

createContainer :: BaseUrl -> ContainerSpec -> IO ContainerReceipt
createContainer bu spec =
  sendRequestJsonResponse bu "containers" (encode spec) JQ.POST

getContainerDetail :: BaseUrl -> Text -> IO ContainerDetail
getContainerDetail bu k =
  sendRequestJsonResponse bu ("containers/" <> encodeURIComponent k) "" JQ.GET

stopContainer :: BaseUrl -> Text -> IO ()
stopContainer bu k =
  sendRequestJsonResponse bu ("containers/" <> encodeURIComponent k) "" JQ.DELETE

getContainerDetailById :: BaseUrl -> ContainerId -> IO ContainerDetail
getContainerDetailById bu cid =
  getContainerDetail bu (cid ^. ciID)

getContainerDetailByReceipt :: BaseUrl -> ContainerReceipt -> IO ContainerDetail
getContainerDetailByReceipt bu cr =
  getContainerDetail bu (pack (UUID.toString (cr ^. crID)))

stopContainerById :: BaseUrl -> ContainerId -> IO ()
stopContainerById bu cid =
  stopContainer bu (cid ^. ciID)

stopContainerByReceipt :: BaseUrl -> ContainerReceipt -> IO ()
stopContainerByReceipt bu cr =
  stopContainer bu (pack (UUID.toString (cr ^. crID)))

pollForContainerAddress :: Int -> IO ContainerDetail -> IO (Text, Int)
pollForContainerAddress n getContainer
  | n <= 0 = fail "Ran out of retries while initializing soh-runner container"
  | otherwise = do
      detail <- getContainer
      case detail ^. cdAddress of
        Nothing -> do
          -- Container is pending - wait a bit and try again.
          threadDelay (1000 * 1000)
          pollForContainerAddress (n - 1) getContainer
        Just address -> return address

mschedulerUrl :: Maybe Text
mschedulerUrl
  | devMode = Nothing
  | otherwise = Just "http://soh-scheduler-1627848338.us-east-1.elb.amazonaws.com"
-- TODO: allow page to determine scheduler URL.
-- | isNull schedulerUrl' || isUndefined schedulerUrl' =
-- | otherwise = Just (fromJSString schedulerUrl')

-- foreign import javascript unsafe
--   "window['schedulerUrl']"
--   schedulerUrl' :: JSString

sendRequestJsonResponse :: Aeson.FromJSON a => BaseUrl -> Text -> JSString -> JQ.Method -> IO a
sendRequestJsonResponse bu route body method =
  decode <$> sendRequest bu route body method

sendRequest :: BaseUrl -> Text -> JSString -> JQ.Method -> IO JSString
sendRequest (BaseUrl bu) route body method =
    ajax (bu <> "/" <> route) body settings
  where
    settings =  JQ.AjaxSettings
      { JQ.asContentType = "application/json"
      , JQ.asCache = False
      , JQ.asIfModified = False
      , JQ.asMethod = method
      }

encode :: Aeson.ToJSON a => a -> JSString
encode = toJSString . decodeUtf8 . toStrict . Aeson.encode

decode :: Aeson.FromJSON a => JSString -> a
decode s =
  case Aeson.eitherDecodeStrict (encodeUtf8 (fromJSString s)) of
    Left e -> error e
    Right x -> x

-- Copied from ghcjs-jquery with the following modifications:
--
-- * Throws errors when status is >= 300.
--
-- * Uses JSStrings instead of converting to and from Text.
--
-- * Sends a raw body rather than parameters.
--
-- * 'Accept' : 'application/json'
--
ajax :: Text -> JSString -> JQ.AjaxSettings -> IO JSString
ajax url d s = do
  os <- toJSRef s
  setProp ("data"::JSString) d os
  setProp ("processData"::JSString) (toJSBool False) os
  headers <- newObj
  setProp ("headers"::JSString) headers os
  setProp ("Accept"::JSString) ("application/json"::JSString) headers
  arr <- JQ.jq_ajax (toJSString url) os
  status <- fromMaybe 0 <$> (fromJSRef =<< getProp ("status"::JSString) arr)
  if status >= 300
    then do
      statusText <- fromMaybe "" <$> (fromJSRef =<< getProp ("statusText"::JSString) arr)
      throwIO (AjaxException status statusText)
    else getProp ("data"::JSString) arr

data AjaxException = AjaxException
  { aeStatus :: Int
  , aeStatusText :: Text
  } deriving (Show, Typeable)

instance Exception AjaxException

encodeURIComponent :: Text -> Text
encodeURIComponent = fromJSString . encodeURIComponent' . toJSString

foreign import javascript unsafe "encodeURIComponent"
  encodeURIComponent' :: JSString -> JSString
