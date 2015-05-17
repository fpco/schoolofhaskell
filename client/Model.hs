module Model where

import           Communication
import           Control.Exception (catch, throwIO, SomeException)
import           Control.Monad (when)
import qualified Data.ByteString.Lazy as BL
import           Data.Function (on)
import           Data.List (partition)
import qualified Data.List as L
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           GHCJS.Types (JSString)
import           GHCJS.Foreign (toJSString)
import           Import
import qualified React.Ace as Ace
import           React.Internal (appState)
import qualified React.TermJs as TermJs

getApp :: IO App
getApp = do
  ace <- Ace.getDef
  termjs <- TermJs.getDef
  let state = State
        { _stateAce = ace
        , _stateStatus = Nothing
        , _stateRunning = NotRunning
        , _stateTab = BuildTab
        , _stateDocs = Nothing
        , _stateTypes = Nothing
        , _stateConsole = termjs
        , _stateBackend = Nothing
        }
  makeApp state id

runApp :: App -> IO void
runApp app = withUrl "ws://localhost:3000/editor" $ \backend -> do
  setTVarIO (appState app) stateBackend (Just backend)
  version <- expectWelcome backend
  putStrLn $ "Backendection established with ide-backend " ++ show version
  let state = appState app
  files <- waitForTVarIO state (^? (stateStatus . _Just . _BuildRequested))
  mainLoop backend state files `catch` \ex -> do
    consoleError $ toJSString $
      "Exited mainLoop with exception " ++ show (ex :: SomeException)
    throwIO ex

mainLoop :: Backend -> TVar State -> Files -> IO void
mainLoop backend state files = do
  success <- compileCode backend state files
  when success $ runConsole backend state
  files' <- runQueries backend state
  mainLoop backend state files'

compileCode :: Backend -> TVar State -> Files -> IO Bool
compileCode backend state files = do
  --TODO: clear ide-backend state before the rest of the updates.
  let requestUpdate (fp, txt) = RequestUpdateSourceFile fp $
        BL.fromStrict (encodeUtf8 txt)
  -- Show the build's progress and wait for it to finish.
  updateSession backend
    (map requestUpdate files)
    (setTVarIO state stateStatus . Just . Building)
  -- Retrieve the errors
  sourceErrors <- getSourceErrors backend
  let partitionKind k = partition ((==k) . errorKind)
  let (errors, partitionKind KindWarning -> (warnings, serverDieds)) =
        partitionKind KindError sourceErrors
  setTVarIO state stateStatus $ Just $ Built $ BuildInfo
    { buildErrors = errors
    , buildWarnings = warnings
    , buildServerDieds = serverDieds
    }
  return (null errors && null serverDieds)

runConsole :: Backend -> TVar State -> IO ()
runConsole backend state = do
  switchToConsoleFirstTime <- once $ setTVarIO state stateTab ConsoleTab
  let appendConsole x = do
        TermJs.TermJs terminal <- viewTVarIO state stateConsole
        TermJs.writeTerminal terminal x
  setProcessHandler backend $ \case
    Right output -> do
      switchToConsoleFirstTime
      appendConsole (toJSString (decodeUtf8 output))
    Left result -> do
      appendConsole ("\nProcess done: " :: JSString)
      appendConsole (toJSString (show result))
  requestRun backend "Main" "main"

runQueries :: Backend -> TVar State -> IO Files
runQueries backend state = do
    req <- waitForUserRequest
    case req of
      Left files -> return files
      Right (QueryInfo ss) -> do
        infos <- getSpanInfo backend ss
        navigateDoc state $
          fmap (\(ResponseSpanInfo si _) -> getIdInfo si)
               (listToMaybe infos)
        -- FIXME: allow the client to restrict their
        -- ide-backend-client request.
        tys <- getAnnExpTypes backend ss
        update $ set stateTypes $
          listToMaybe $
          L.groupBy ((==) `on` (\(ResponseAnnExpType _ _ ss') -> ss')) tys
        runQueries backend state
  where
    waitForUserRequest :: IO (Either Files Query)
    waitForUserRequest = waitForTVarIO state $ \s ->
      case s ^. stateStatus of
        Just (BuildRequested files) -> Just (Left files)
        Just (QueryRequested _ query) -> Just (Right query)
        _ -> Nothing
    update :: (State -> State) -> IO ()
    update f = modifyTVarIO state id $ over stateStatus backToIdle . f
    backToIdle :: Maybe Status -> Maybe Status
    backToIdle (Just (QueryRequested info _)) = Just (Built info)
    backToIdle x = x
    getIdInfo (SpanId x) = x
    getIdInfo (SpanQQ x) = x

--------------------------------------------------------------------------------
-- Mutation functions invoked by View

runCode :: TVar State -> [(FilePath, Text)] -> IO ()
runCode state files =
  modifyTVarIO state stateStatus $ \oldStatus ->
    case oldStatus of
      Just (Building _) -> oldStatus
      _ -> Just $ BuildRequested files

runQuery :: TVar State -> Query -> IO ()
runQuery state query =
  modifyTVarIO state stateStatus $ \oldStatus ->
    case oldStatus of
      Just (Built info) -> Just $ QueryRequested info query
      _ -> oldStatus

navigateDoc :: TVar State -> Maybe IdInfo -> IO ()
navigateDoc state = setTVarIO state stateDocs

switchTab :: TVar State -> Tab -> IO ()
switchTab state = setTVarIO state stateTab
