-- | This module provides an API for interacting with
-- ide-backend-client over the websocket interface provided by
-- soh-runner.
--
-- This API wraps up the different patterns of sending requests and
-- expecting responses, such that the code which uses it mostly does
-- not need to worry about violating any protocol invariants.
--
-- The only current invariant that needs to be preserved is that all
-- the functions which expect a response can't be executed
-- concurrently.    In particular, this applies to all of the queries,
-- 'updateSession', and 'expectWelcome'.  Process starting, stdin,
-- stdout, and killing can all be done concurrently.
--
-- In the future, a runtime check for this might be added.  However,
-- for now this is enforced by the single-threaded nature of "Model".
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
  -- * Runner commands
  , requestPortListening
  ) where

import           Control.Concurrent.Async (race)
import           Control.Concurrent.STM
import           Data.Aeson (eitherDecodeStrict, encode)
import           Data.ByteString.Lazy (toStrict)
import           Data.Function (fix)
import           Data.IORef
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8, decodeASCII)
import qualified Data.UUID.Types as UUID
import           Data.Void (absurd)
import           Import
import qualified JavaScript.WebSockets as WS
import           Language.JsonGrammar (Json)
import           SchoolOfHaskell.RunnerAPI
import           SchoolOfHaskell.Scheduler.API (ContainerReceipt(..))

-- | Given the URL of the SoH container, this creates a websockets
-- connection to it.
withUrl :: Text -> Int -> ContainerReceipt -> (Backend -> IO a) -> IO a
withUrl backendHost port (ContainerReceipt uuid) f =
  let url = "ws://" <> backendHost <> ":" <> tshow port in
  WS.withUrl url $ \conn -> do
    -- Send the receipt to the backend.  If it's rejected, then an
    -- exception is thrown.
    let receiptText = decodeASCII (UUID.toASCIIBytes uuid)
    sendJson conn (RunnerRequestAuth receiptText)
    authResponse <- receiveJson conn
    case authResponse of
      RunnerResponseAuthSuccess -> return ()
      _ -> fail "Didn't receive expected authentication success from runner."
    -- Initialize state of the 'Backend' type, and fork off threads for
    -- handling communication with the backend.
    backendRequestChan <- newTChanIO
    backendResponseChan <- newTChanIO
    backendProcessHandler <- newIORef $ \_ ->
      consoleWarnText "backendProcessHandler not yet set"
    let sendThread = showExceptions "sendThread" $ forever $
          atomically (readTChan backendRequestChan) >>= sendJson conn
        receiveThread = showExceptions "receiveThread" $ forever $ do
          response <- receiveJson conn
          case response of
            RunnerResponseAuthSuccess ->
              fail "Didn't expect to receive auth response while running"
            RunnerResponseAuthFailure ->
              fail "Didn't expect to receive auth response while running"
            RunnerResponsePortIsListening ->
              readIORef backendProcessHandler >>= ($ ProcessListening)
            RunnerResponseClient response' ->
              case response' of
                ResponseProcessOutput bs ->
                  readIORef backendProcessHandler >>= ($ ProcessOutput bs)
                ResponseProcessDone rr ->
                  readIORef backendProcessHandler >>= ($ ProcessDone rr)
                ResponseNoProcessError ->
                  consoleErrorText "No running process"
                _ -> atomically (writeTChan backendResponseChan response')
    result <- receiveThread `race` sendThread `race` f Backend {..}
    case result of
      Left (Left x) -> absurd x
      Left (Right x) -> absurd x
      Right x -> return x

--------------------------------------------------------------------------------
-- Commands

-- | Sends updates to the backend.  The backend will send back
-- progress updates until it finishes compilation.  These progress
-- updates are provided to the callback function.  Once compilation is
-- finished, 'Nothing' is sent to the callback and this function
-- returns.
updateSession :: Backend -> [RequestSessionUpdate] -> (Maybe Progress -> IO ()) -> IO ()
updateSession backend updates f = do
  sendRequest backend (RequestUpdateSession updates)
  fix $ \loop -> do
    mx <- expectResponse backend
                         (^? _ResponseUpdateSession)
                         "ResponseUpdateSession"
    f mx
    when (isJust mx) loop

-- | Requests that the backend run the user's code.  The module nad
-- identifier to run are taken as parameters.
requestRun :: Backend -> ModuleName -> Identifier -> IO ()
requestRun backend mn ident = sendRequest backend $ RequestRun True mn ident

--------------------------------------------------------------------------------
-- Queries

-- | Gets the source errors of the last compilation.
getSourceErrors :: Backend -> IO [SourceError]
getSourceErrors backend =
  queryBackend backend
               RequestGetSourceErrors
               _ResponseGetSourceErrors
               "ResponseGetSourceErrors"

-- | Gets the annotated source errors of the last compilation.  These
-- annotations add more structure and info to the error messages, so
-- they can be presented in nicer, more informative ways.
getAnnSourceErrors :: Backend ->  IO [AnnSourceError]
getAnnSourceErrors backend =
  queryBackend backend
               RequestGetAnnSourceErrors
               _ResponseGetAnnSourceErrors
               "ResponseGetAnnSourceErrors"

-- | Gets the span info of the last __error-free__ compile.  Span info
-- tells you where an identifier came from.
getSpanInfo :: Backend -> SourceSpan -> IO [ResponseSpanInfo]
getSpanInfo backend ss =
  queryBackend backend
               (RequestGetSpanInfo ss)
               _ResponseGetSpanInfo
               "ResponseGetSpanInfo"

-- | Gets the type info of the last __error-free__ compile.  This
-- tells you the type info
getExpTypes :: Backend -> SourceSpan -> IO [ResponseExpType]
getExpTypes backend ss =
  queryBackend backend
               (RequestGetExpTypes ss)
               _ResponseGetExpTypes
               "ResponseGetExpTypes"

-- | Gets the annotated type info of the last __error-free__ compile. These
-- annotations add identifier info to the type info, so that doc links
-- can be provided in the type info.
getAnnExpTypes :: Backend -> SourceSpan -> IO [ResponseAnnExpType]
getAnnExpTypes backend ss =
  queryBackend backend
               (RequestGetAnnExpTypes ss)
               _ResponseGetAnnExpTypes
               "ResponseGetAnnExpTypes"

-- Pattern of sending a request and expecting a response, common to
-- the queries above.
queryBackend :: Backend -> Request -> Prism' Response a -> String -> IO a
queryBackend backend request p expected = do
  sendRequest backend request
  expectResponse backend (^? p) expected

--------------------------------------------------------------------------------
-- Process IO

-- | Sets the callback which is used to handle process output.  Stdout
-- is provided as 'Right' values, and the 'Left' values let you know
-- that the process exited.
setProcessHandler :: Backend -> (ProcessOutput -> IO ()) -> IO ()
setProcessHandler = atomicWriteIORef . backendProcessHandler

-- | Sends stdin to the process.
sendProcessInput :: Backend -> ByteString -> IO ()
sendProcessInput backend = sendRequest backend . RequestProcessInput

-- | Sends a SIGINT signal to the process, equivalent of using Ctrl-C.
sendProcessKill :: Backend -> IO ()
sendProcessKill backend = sendRequest backend RequestProcessKill

--------------------------------------------------------------------------------
-- Misc

-- | Expects the welcome message which is sent by ide-backend-client
-- once the connection is established.
expectWelcome :: Backend -> IO VersionInfo
expectWelcome backend =
  expectResponse backend (^? _ResponseWelcome) "ResponseWelcome"

--------------------------------------------------------------------------------
-- SoH Runner Commands

requestPortListening :: Backend -> Int -> IO ()
requestPortListening backend = sendRequest' backend . RunnerRequestPortListening

--------------------------------------------------------------------------------
-- Backend IO

sendRequest :: Backend -> Request -> IO ()
sendRequest backend = sendRequest' backend . RunnerRequestClient

sendRequest' :: Backend -> RunnerRequest -> IO ()
sendRequest' backend = atomically . writeTChan (backendRequestChan backend)

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

--FIXME: fewer conversions...
sendJson :: Json a => WS.Connection -> a -> IO ()
sendJson conn = sendText conn . decodeUtf8 . toStrict . encode . toJSON

sendText :: WS.Connection -> Text -> IO ()
sendText conn req = do
  connected <- WS.sendText conn req
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
