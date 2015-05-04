{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Model where

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever, when, void)
import           Data.Aeson (eitherDecodeStrict, encode)
import           Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (forM_)
import           Data.Function (fix)
import           Data.IORef
import           Data.List (partition)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Import
import           JavaScript.WebSockets
import           Language.JsonGrammar (Json)
import           React.Ace (Ace)
import qualified React.Ace as Ace
import           React.Internal (appState)

$(makePrisms ''Response)

getApp :: IO App
getApp = do
  ace <- Ace.getDef
  let state = State
        { _stateAce = ace
        , _stateStatus = Nothing
        , _stateRunning = NotRunning
        , _stateTab = BuildTab
        , _stateConsole = []
        }
  makeApp state id

runApp :: App -> IO ()
runApp app = withUrl "ws://localhost:3000/editor" $ \conn' -> do
  let conn = BackendConnection conn'
  version <- receiveResponse conn passthrough _ResponseWelcome
    "handshake response (ResponseWelcome)"
  putStrLn $ "Connection established with ide-backend " ++ show version
  mainLoop conn (appState app)

mainLoop :: BackendConnection -> TVar State -> IO ()
mainLoop conn state = forever $ do
  -- When a build is requested, update the backend's session.
  files <- waitForTVarIO state (stateStatus . _Just . _BuildRequested)
  putStrLn "Got build request"
  --TODO: clear ide-backend state before the rest of the updates.
  let requestUpdate (fp, txt) = RequestUpdateSourceFile fp $
        BL.fromStrict (encodeUtf8 txt)
  sendRequest conn $ RequestUpdateSession $ map requestUpdate files
  -- Show the build's progress and wait for it to finish.
  fix $ \loop -> do
    mprogress <- receiveResponse conn passthrough _ResponseUpdateSession
      "compilation progress (ResponseUpdateSession)"
    setTVarIO state stateStatus (Just (Building mprogress))
    forM_ mprogress (\_ -> loop)
  -- Retrieve the errors
  sourceErrors <- getSourceErrors conn passthrough
  let partitionKind k = partition ((==k) . errorKind)
  let (errors, partitionKind KindWarning -> (warnings, serverDieds)) =
        partitionKind KindError sourceErrors
  setTVarIO state stateStatus $ Just $ Built $ BuildInfo
    { buildErrors = errors
    , buildWarnings = warnings
    , buildServerDieds = serverDieds
    }
  ih <- if not (null errors && null serverDieds)
    then return passthrough
    -- If there aren't any errors, run the process.
    else do
      setTVarIO state stateTab ConsoleTab
      sendRequest conn $ RequestRun "Main" "main"
      return $ \response ->
        case response of
          ResponseProcessOutput output -> do
            modifyTVarIO state stateConsole $ (++ [decodeUtf8 output])
            return HandledResponse
          ResponseProcessDone result -> do
            modifyTVarIO state stateConsole $
              (++ ["\nProcess done: ", T.pack (show result)])
            return HandledResponse
          _ -> return Didn'tHandleResponse
  forever $ do
    void $ receiveResponse conn ih id "heh"
    threadDelay (100 * 1000)

getSourceErrors :: BackendConnection
                -> (Response -> IO HandledResponse)
                -> IO [SourceError]
getSourceErrors conn ihr =
  query conn ihr RequestGetSourceErrors _ResponseGetSourceErrors
     "ResponseGetSourceErrors"

query :: BackendConnection
      -> (Response -> IO HandledResponse)
      -> Request
      -> Prism' Response a
      -> String
      -> IO a
query conn ih request prism expected = do
  sendRequest conn request
  receiveResponse conn ih prism expected

--------------------------------------------------------------------------------
-- Functions for sending and receiving JSON

sendRequest :: BackendConnection -> Request -> IO ()
sendRequest = sendJson . unBackendConnection

receiveResponse :: BackendConnection
                -> (Response -> IO HandledResponse)
                -> Prism' Response a
                -> String
                -> IO a
receiveResponse = receiveJson . unBackendConnection

--------------------------------------------------------------------------------
-- Functions for sending and receiving JSON

sendJson :: Json a => Connection -> a -> IO ()
sendJson conn req =
  sendText_ conn (decodeUtf8 (toStrict (encode (toJSON req))))

data HandledResponse
    = HandledResponse
    | Didn'tHandleResponse
    deriving (Eq)

passthrough :: a -> IO HandledResponse
passthrough _ = return Didn'tHandleResponse

receiveJson :: (Json r, Show r)
            => Connection
            -> (r -> IO HandledResponse)
            -> Prism' r a
            -> String
            -> IO a
receiveJson conn ih prism expected = do
  t <- receiveText_ conn
  case eitherDecodeStrict (encodeUtf8 t) of
    Left err -> fail $ "Protocol error: " ++ err
    Right val ->
      case fromJSON val of
        Left err -> fail $ "Protocol error: " ++ err
        Right response -> do
          handled <- ih response
          case handled of
            HandledResponse -> receiveJson conn ih prism expected
            Didn'tHandleResponse ->
              case response ^? prism of
                Just x -> return x
                Nothing ->
                  fail $
                    "Protocol error, expected " ++ expected ++
                    ", but got " ++ show response

--------------------------------------------------------------------------------
-- Mutation functions invoked by View

runCode :: [(FilePath, Text)] -> TVar State -> IO ()
runCode files state =
  modifyTVarIO state stateStatus $ \oldStatus ->
    case oldStatus of
      Just (Building _) -> oldStatus
      _ -> Just $ BuildRequested files

switchTab :: Tab -> TVar State -> IO ()
switchTab tab state =
  setTVarIO state stateTab tab
