-- | This module defines the School of Haskell client's behavior.  It
-- handles updating the state based on responses from the backend, and
module Model where

import           Communication
import           Control.Exception (catch, throwIO, SomeException)
import qualified Data.ByteString.Lazy as BL
import           Data.Function (on)
import           Data.List (partition)
import qualified Data.List as L
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Vector as V
import           Import
import           PosMap (emptyPosMap)
import           React.IFrame (setIFrameUrl)
import           React.Internal (appState)
import           SchoolOfHaskell.Scheduler.API (ContainerReceipt)
import           TermJs (writeTerminal)

-- | Given the number of snippets on the page, this creates the
-- initial state and App.  It needs to know the number of snippets in
-- order to initialize the Ace components.
getApp :: Int -> IO App
getApp cnt = do
  snippets <- V.replicateM cnt $ do
    editor <- getDefUnmanaged
    return Snippet
      { _snippetEditor = editor
      , _snippetPosMap = emptyPosMap
      , _snippetTypeInfo = Nothing
      }
  termjs <- getDefUnmanaged
  web <- getDefUnmanaged
  let state = State
        { _stateSnippets = snippets
        , _stateConsole = termjs
        , _stateWeb = web
        , _stateStatus = InitialStatus
        , _stateRunning = NotRunning
        , _stateTab = BuildTab
        , _stateDocs = Nothing
        , _stateBackend = Nothing
        }
  makeApp state id

-- | Runs the SoH client application.
runApp :: Text -> Int -> ContainerReceipt -> App -> IO void
runApp host port receipt app = withUrl host port receipt $ \backend -> do
  setTVarIO (appState app) stateBackend (Just backend)
  version <- expectWelcome backend
  putStrLn $ "Connection established with ide-backend " ++ show version
  let state = appState app
      -- TODO: Other env variables from old SoH?  APPROOT,
      -- FP_ENVIRONMENT_NAME, FP_ENVIRONMENT_TYPE, IMAGE_DIR, and etc
      initialUpdates = [RequestUpdateEnv [("PORT", Just (show webPort))]]
  br <- waitForTVarIO state (^? (stateStatus . _BuildRequested))
  mainLoop backend state br initialUpdates `catch` \ex -> do
    consoleErrorText $
      "Exited mainLoop with exception " <> tshow (ex :: SomeException)
    throwIO ex

-- | This is the main loop.  It takes the current 'BuildRequest',
-- compiles the code, runs it, and then waits for queries or further
-- build requests.  These build requests re-enter this 'mainLoop'
-- function.  As is implied by its @IO void@ return type, it never
-- returns.
mainLoop
  :: Backend
  -> TVar State
  -> BuildRequest
  -> [RequestSessionUpdate]
  -> IO void
mainLoop backend state br extraUpdates = do
  (sid, bi) <- compileCode backend state br extraUpdates
  -- Kill the running process, if there is one.
  killProcess backend state sid bi
  when (buildSuccess bi) $ runConsole backend state
  br' <- runQueries backend state
  mainLoop backend state br' []

-- | Whether there are no errors in a 'BuildInfo'.
buildSuccess :: BuildInfo -> Bool
buildSuccess bi = null (buildErrors bi) && null (buildServerDieds bi)

-- | Compiles a set of files and retrieves the resulting error
-- / warning messages.
compileCode
  :: Backend
  -> TVar State
  -> BuildRequest
  -> [RequestSessionUpdate]
  -> IO (SnippetId, BuildInfo)
compileCode backend state (BuildRequest sid files) extraUpdates = do
  --TODO: clear ide-backend state before the rest of the updates.
  let requestUpdate (fp, txt) = RequestUpdateSourceFile fp $
        BL.fromStrict (encodeUtf8 txt)
  -- Show the build's progress and wait for it to finish.
  updateSession backend
    (extraUpdates ++ map requestUpdate files)
    (setTVarIO state stateStatus . Building sid)
  -- Retrieve the errors
  sourceErrors <- getAnnSourceErrors backend
  let partitionKind k = partition ((==k) . annErrorKind)
  let (errors, partitionKind KindWarning -> (warnings, serverDieds)) =
        partitionKind KindError sourceErrors
      buildInfo = BuildInfo
        { buildErrors = errors
        , buildWarnings = warnings
        , buildServerDieds = serverDieds
        }
  setTVarIO state stateStatus $ Built sid buildInfo
  return (sid, buildInfo)

-- | Runs the user's program and directs stdout to the console.
runConsole :: Backend -> TVar State -> IO ()
runConsole backend state = do
  switchTab state ConsoleTab
  let appendConsole x = do
        terminal' <- readUnmanagedOrFail state (^? stateConsole)
        writeTerminal terminal' x
  setProcessHandler backend $ \case
    ProcessOutput output -> appendConsole (decodeUtf8 output)
    ProcessDone result ->
      appendConsole $ "\r\nProcess done: " <> T.pack (show result) <> "\r\n"
    ProcessListening -> do
      webFrame <- readUnmanagedOrFail state (^? stateWeb)
      let url = "http://" <> backendHost backend <> ":" <> tshow webPort
      setIFrameUrl webFrame url
      switchTab state WebTab
  requestRun backend "Main" "main"
  requestPortListening backend webPort

-- | Which port is used to serve websites from snippets.
webPort :: Int
webPort = 3000

-- | Waits for queries and performs them.  Once a build is requested
-- this stops waiting for queries and yields the 'BuildRequest'.
runQueries :: Backend -> TVar State -> IO BuildRequest
runQueries backend state = do
    req <- waitForUserRequest
    case req of
      Left br -> return br
      Right (sid, QueryInfo ss) -> do
        infos <- getSpanInfo backend ss
        navigateDoc state $
          fmap (\(ResponseSpanInfo si _) -> getIdInfo si)
               (listToMaybe infos)
        -- FIXME: allow the client to restrict their
        -- ide-backend-client request.
        tys <- getAnnExpTypes backend ss
        update $ set (ixSnippet sid . snippetTypeInfo) $
          listToMaybe $
          L.groupBy ((==) `on` (\(ResponseAnnExpType _ ss') -> ss')) tys
        runQueries backend state
  where
    waitForUserRequest :: IO (Either BuildRequest (SnippetId, Query))
    waitForUserRequest = waitForTVarIO state $ \s ->
      case s ^. stateStatus of
        BuildRequested br -> Just (Left br)
        QueryRequested sid _ query -> Just (Right (sid, query))
        _ -> Nothing
    update :: (State -> State) -> IO ()
    update f = modifyTVarIO state id $ over stateStatus backToIdle . f
    backToIdle :: Status -> Status
    backToIdle (QueryRequested sid info _) = Built sid info
    backToIdle x = x
    getIdInfo (SpanId x) = x
    getIdInfo (SpanQQ x) = x

-- | Send process kill request, and wait for the process to stop.
--
-- Note: the status on invoking this ought to be 'Built'.  The fields
-- of 'Built' are passed in to the function.
killProcess :: Backend -> TVar State -> SnippetId -> BuildInfo -> IO ()
killProcess backend state sid bi = do
  sendProcessKill backend
  setTVarIO state stateStatus (KillRequested sid bi)
  waitForTVarIO state $ \s ->
    if (s ^. stateRunning) == NotRunning then Just () else Nothing
  setTVarIO state stateStatus (Built sid bi)

--------------------------------------------------------------------------------
-- Mutation functions invoked by View (a.k.a. "the controller")

-- | Runs the user's code.
runCode :: TVar State -> BuildRequest -> IO ()
runCode state br@(BuildRequest sid _) = atomically $ modifyTVar state $ \s ->
  case s ^. stateStatus of
    Building _ _ -> s
    _ -> s & stateStatus .~ BuildRequested br
           & (ixSnippet sid . snippetPosMap) .~ emptyPosMap

-- | Runs a query.
runQuery :: TVar State -> SnippetId -> Query -> IO ()
runQuery state sid query =
  modifyTVarIO state stateStatus $ \oldStatus ->
    case oldStatus of
      --TODO: Consider whether we want some other behavior when a
      --query is requested for a non-current snippet.  Seems like we
      --should let the user know why the query isn't being performed.
      Built sid' info | sid' == sid -> QueryRequested sid info query
      _ -> oldStatus

-- | Sets the id-info which the haddock iframe should use for its url.
navigateDoc :: TVar State -> Maybe IdInfo -> IO ()
navigateDoc state = setTVarIO state stateDocs

-- | Switches which tab is currently focused.
switchTab :: TVar State -> Tab -> IO ()
switchTab state = setTVarIO state stateTab
