module Communication
  ( Backend
  , withUrl
  -- * Commands
  , updateSession
  , requestRun
  -- * Queries
  , getSourceErrors
  , getAnnSourceErrors
  , getSpanInfo
  , getExpTypes
  , getAnnExpTypes
  -- * Process IO
  , setProcessHandler
  , sendProcessInput
  , sendProcessKill
  -- * Misc
  , expectWelcome
  ) where

import           Control.Concurrent.Async (race)
import           Control.Concurrent.STM
import           Data.Aeson (eitherDecodeStrict, encode)
import           Data.ByteString.Lazy (toStrict)
import           Data.Function (fix)
import           Data.IORef
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Void (absurd)
import           GHCJS.Types (JSString)
import           Import
import qualified JavaScript.WebSockets as WS
import           Language.JsonGrammar (Json)

withUrl :: Text -> (Backend -> IO a) -> IO a
withUrl url f = WS.withUrl url $ \conn -> do
  backendRequestChan <- newTChanIO
  backendResponseChan <- newTChanIO
  backendProcessHandler <- newIORef $ \_ ->
    consoleWarnText "backendProcessHandler not yet set"
  let sendThread = showExceptions "sendThread" $ forever $
        atomically (readTChan backendRequestChan) >>= sendJson conn
      receiveThread = forever $ do
        response <- receiveJson conn
        case response of
          ResponseProcessOutput bs ->
            readIORef backendProcessHandler >>= ($ Right bs)
          ResponseProcessDone rr ->
            readIORef backendProcessHandler >>= ($ Left rr)
          _ -> atomically (writeTChan backendResponseChan response)
  result <- receiveThread `race` sendThread `race` f Backend {..}
  case result of
    Left (Left x) -> absurd x
    Left (Right x) -> absurd x
    Right x -> return x

--------------------------------------------------------------------------------
-- Commands

updateSession :: Backend -> [RequestSessionUpdate] -> (Maybe Progress -> IO ()) -> IO ()
updateSession backend updates f = do
  sendRequest backend (RequestUpdateSession updates)
  fix $ \loop -> do
    mx <- expectResponse backend
                         (^? _ResponseUpdateSession)
                         "ResponseUpdateSession"
    f mx
    when (isJust mx) loop

requestRun :: Backend -> ModuleName -> Identifier -> IO ()
requestRun backend mn ident = sendRequest backend $ RequestRun True mn ident

--------------------------------------------------------------------------------
-- Queries

getSourceErrors :: Backend -> IO [SourceError]
getSourceErrors backend =
  queryBackend backend
               RequestGetSourceErrors
               _ResponseGetSourceErrors
               "ResponseGetSourceErrors"

getAnnSourceErrors :: Backend ->  IO [AnnSourceError]
getAnnSourceErrors backend =
  queryBackend backend
               RequestGetAnnSourceErrors
               _ResponseGetAnnSourceErrors
               "ResponseGetAnnSourceErrors"

getSpanInfo :: Backend -> SourceSpan -> IO [ResponseSpanInfo]
getSpanInfo backend ss =
  queryBackend backend
               (RequestGetSpanInfo ss)
               _ResponseGetSpanInfo
               "ResponseGetSpanInfo"

getExpTypes :: Backend -> SourceSpan -> IO [ResponseExpType]
getExpTypes backend ss =
  queryBackend backend
               (RequestGetExpTypes ss)
               _ResponseGetExpTypes
               "ResponseGetExpTypes"

getAnnExpTypes :: Backend -> SourceSpan -> IO [ResponseAnnExpType]
getAnnExpTypes backend ss =
  queryBackend backend
               (RequestGetAnnExpTypes ss)
               _ResponseGetAnnExpTypes
               "ResponseGetAnnExpTypes"

queryBackend :: Backend -> Request -> Prism' Response a -> String -> IO a
queryBackend backend request p expected = do
  sendRequest backend request
  expectResponse backend (^? p) expected

--------------------------------------------------------------------------------
-- Process IO

setProcessHandler :: Backend -> ((Either RunResult ByteString) -> IO ()) -> IO ()
setProcessHandler = atomicWriteIORef . backendProcessHandler

sendProcessInput :: Backend -> ByteString -> IO ()
sendProcessInput backend = sendRequest backend . RequestProcessInput

sendProcessKill :: Backend -> IO ()
sendProcessKill backend = sendRequest backend RequestProcessKill

--------------------------------------------------------------------------------
-- Misc

expectWelcome :: Backend -> IO VersionInfo
expectWelcome backend =
  expectResponse backend (^? _ResponseWelcome) "ResponseWelcome"

--------------------------------------------------------------------------------
-- Backend IO

sendRequest :: Backend -> Request -> IO ()
sendRequest backend = atomically . writeTChan (backendRequestChan backend)

receiveResponse :: Backend -> IO Response
receiveResponse = atomically . readTChan . backendResponseChan

expectResponse :: Backend -> (Response -> Maybe a) -> String -> IO a
expectResponse backend f expected = do
  response <- receiveResponse backend
  case f response of
    Nothing -> fail $
      "Protocol error: expected " ++ expected ++
      " instead of " ++ show response
    Just x -> return x

--------------------------------------------------------------------------------
-- Sending and receiving JSON

sendJson :: Json a => WS.Connection -> a -> IO ()
sendJson conn req = do
  --FIXME: fewer conversions...
  connected <- WS.sendText conn (decodeUtf8 (toStrict (encode (toJSON req))))
  when (not connected) $ fail "Websocket disconnected"

receiveJson :: Json a => WS.Connection -> IO a
receiveJson conn = do
  t <- WS.receiveText conn
  case eitherDecodeStrict (encodeUtf8 t) of
    Left err -> fail $ "JSON decode error: " ++ err
    Right json ->
      case fromJSON json of
        Left err -> fail $ "JSON deserialization error: " ++ err
        Right x -> return x
