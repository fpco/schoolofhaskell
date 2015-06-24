-- | Handles communicating with the server, to initialize the SoH
-- container.
module ContainerClient
  ( listContainers
  , createContainer
  , getContainerDetailById
  , getContainerDetailByReceipt
  , stopContainerById
  , stopContainerByReceipt
  , pollForContainerAddress
  , schedulerHost
  , lookupPort
  , AjaxException(..)
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Exception (throwIO, Exception)
import           Control.Lens
import qualified Data.Aeson as Aeson
import           Data.ByteString.Lazy (toStrict)
import           Data.List (find)
import           Data.Text (pack)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Typeable (Typeable)
import qualified Data.UUID.Types as UUID
import           Import
import qualified JavaScript.JQuery as JQ
import qualified JavaScript.JQuery.Internal as JQ
import           SchoolOfHaskell.Scheduler.API

listContainers :: IO [ContainerId]
listContainers =
  sendRequestJsonResponse "containers" "" JQ.GET

createContainer :: ContainerSpec -> IO ContainerReceipt
createContainer spec =
  sendRequestJsonResponse "containers" (encode spec) JQ.POST

getContainerDetail :: Text -> IO ContainerDetail
getContainerDetail k =
  sendRequestJsonResponse ("containers/" <> encodeURIComponent k) "" JQ.GET

stopContainer :: Text -> IO ()
stopContainer k =
  sendRequestJsonResponse ("containers/" <> encodeURIComponent k) "" JQ.DELETE

getContainerDetailById :: ContainerId -> IO ContainerDetail
getContainerDetailById cid =
  getContainerDetail (cid ^. ciID)

getContainerDetailByReceipt :: ContainerReceipt -> IO ContainerDetail
getContainerDetailByReceipt cr =
  getContainerDetail (pack (UUID.toString (cr ^. crID)))

stopContainerById :: ContainerId -> IO ()
stopContainerById cid =
  stopContainer (cid ^. ciID)

stopContainerByReceipt :: ContainerReceipt -> IO ()
stopContainerByReceipt cr =
  stopContainer (pack (UUID.toString (cr ^. crID)))

pollForContainerAddress :: Int -> IO ContainerDetail -> IO (Text, PortMappings)
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

-- TODO: allow page to determine scheduler Host.
-- | isNull schedulerHost' || isUndefined schedulerHost' =
-- | otherwise = Just (fromJSString schedulerHost')
--
-- foreign import javascript unsafe
--   "window['schedulerHost']"
--   schedulerHost' :: JSString

-- FIXME: when looking up the backend port, there is no reasonable
-- recovery if it isn't in the association list.  So, once we have
-- logic for connection retry, this will need to be a variety of
-- exception which aborts retry.
--
-- (Not a big deal though, it shouldn't occur).
lookupPort :: Int -> PortMappings -> Int
lookupPort innerPort (PortMappings xs) =
  fromMaybe (error ("Couldn't find port mapping for " ++ show innerPort))
            (snd <$> find ((innerPort ==) . fst) xs)


sendRequestJsonResponse :: Aeson.FromJSON a => Text -> JSString -> JQ.Method -> IO a
sendRequestJsonResponse route body method =
  decode <$> sendRequest route body method

sendRequest :: Text -> JSString -> JQ.Method -> IO JSString
sendRequest route body method =
    ajax (schedulerHost <> "/" <> route) body settings
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
