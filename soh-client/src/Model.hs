-- | This module defines the School of Haskell client's behavior.  It
-- handles updating the state based on responses from the backend, and
module Model where

import           Control.Exception (catch, throwIO, SomeException)
import           Data.Function (on)
import           Data.List (partition)
import qualified Data.List as L
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V
import           Import
import qualified JavaScript.Ace as Ace
import           JavaScript.IFrame (setIFrameUrl)
import           JavaScript.TermJs (writeTerminal)
import           Model.Protocol
import           Model.Server (lookupPort)
import           View.PosMap (emptyPosMap, spanToRange)
import           React.Internal (appState)
import           SchoolOfHaskell.Scheduler.API (ContainerReceipt, PortMappings)

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
  docs <- getDefUnmanaged
  let state = State
        { _stateSnippets = snippets
        , _stateConsole = termjs
        , _stateWeb = web
        , _stateDocs = docs
        , _stateStatus = InitialStatus
        , _stateRunning = NotRunning
        , _stateTab = BuildTab
        , _stateBackend = Nothing
        }
  makeApp state id

-- | Runs the SoH client application.
runApp :: Text -> PortMappings -> ContainerReceipt -> App -> IO void
runApp host ports receipt app = withUrl host ports receipt $ \backend -> do
  setTVarIO (appState app) stateBackend (Just backend)
  version <- expectWelcome backend
  putStrLn $ "Connection established with ide-backend " ++ show version
  let state = appState app
  br <- waitForTVarIO state (^? (stateStatus . _BuildRequested))
  mainLoop backend state br `catch` \ex -> do
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
  -> IO void
mainLoop backend state br = do
  (sid, bi) <- compileCode backend state br
  -- Kill the running process, if there is one.
  killProcess backend state sid bi
  when (buildSuccess bi) $ runConsole backend state
  br' <- runQueries backend state
  mainLoop backend state br'

-- | Whether there are no errors in a 'BuildInfo'.
buildSuccess :: BuildInfo -> Bool
buildSuccess bi = null (buildErrors bi) && null (buildServerDieds bi)

-- | Compiles a set of files and retrieves the resulting error
-- / warning messages.
compileCode
  :: Backend
  -> TVar State
  -> BuildRequest
  -> IO (SnippetId, BuildInfo)
compileCode backend state (BuildRequest sid files) = do
  -- TODO: clear ide-backend state before the rest of the updates?
  let requestUpdate (fp, txt) = RequestUpdateSourceFile fp $
        ByteString64 (encodeUtf8 txt)
  -- Show the build's progress and wait for it to finish.
  updateSession backend
    (map requestUpdate files)
    (setTVarIO state stateStatus . Building sid)
  -- Retrieve the errors
  sourceErrors <- getSourceErrors backend
  let partitionKind k = partition ((==k) . errorKind)
  let (errors, partitionKind KindWarning -> (warnings, serverDieds)) =
        partitionKind KindError sourceErrors
      buildInfo = BuildInfo
        { buildErrors = errors
        , buildWarnings = warnings
        , buildServerDieds = serverDieds
        }
  setTVarIO state stateStatus $ Built sid buildInfo
  when (not (buildSuccess buildInfo)) $ switchTab state BuildTab
  return (sid, buildInfo)

-- | Runs the user's program and directs stdout to the console.
runConsole :: Backend -> TVar State -> IO ()
runConsole backend state = do
  switchTab state ConsoleTab
  port <- requestOpenPort backend
  -- TODO: Other env variables from old SoH? APPROOT,
  -- FP_ENVIRONMENT_NAME, FP_ENVIRONMENT_TYPE, IMAGE_DIR, and etc
  updateSession backend
    [RequestUpdateEnv [("PORT", Just (show port))]]
    (\_ -> return ())
  let appendConsole x = do
        terminal' <- readUnmanagedOrFail state (^? stateConsole)
        writeTerminal terminal' x
  setProcessHandler backend $ \case
    ProcessOutput output -> appendConsole (T.pack output)
    ProcessDone result -> do
      setTVarIO state stateRunning NotRunning
      appendConsole $ "\r\nProcess done: " <> T.pack (show result) <> "\r\n"
    ProcessListening -> do
      webFrame <- readUnmanagedOrFail state (^? stateWeb)
      let url = "http://" <> backendHost backend <> ":" <> tshow port
      setIFrameUrl webFrame url
      switchTab state WebTab
  requestRun backend "Main" "main"
  setTVarIO state stateRunning Running
  requestPortListening backend port

-- | Waits for queries and performs them.  Once a build is requested
-- this stops waiting for queries and yields the 'BuildRequest'.
runQueries :: Backend -> TVar State -> IO BuildRequest
runQueries backend state = do
    req <- waitForUserRequest
    case req of
      Left br -> return br
      Right (sid, QueryInfo ss) -> do
        runDocQuery backend state ss
        mtys <- runTypeQuery backend state sid ss
        update $ set (ixSnippet sid . snippetTypeInfo) mtys
        -- TODO: allow the client to restrict their stack-ide request to
        -- just the innermost type info.
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

runDocQuery :: Backend -> TVar State -> SourceSpan -> IO ()
runDocQuery backend state ss = do
    infos <- getSpanInfo backend ss
    navigateDoc state $
      fmap (\(ResponseSpanInfo si _) -> getIdInfo si)
           (listToMaybe infos)
  where
    getIdInfo (SpanId x) = x
    getIdInfo (SpanQQ x) = x

runTypeQuery :: Backend -> TVar State -> SnippetId -> SourceSpan -> IO (Maybe ([ResponseAnnExpType], Int, Int, Ace.MarkerId))
runTypeQuery backend state sid ss = do
    tys <- getAnnExpTypes backend ss
    addTyPos $
      listToMaybe $
      L.groupBy ((==) `on` (\(ResponseAnnExpType _ ss') -> ss')) tys
  where
    addTyPos Nothing = return Nothing
    addTyPos (Just []) = return Nothing
    addTyPos (Just tys@((ResponseAnnExpType _ ss'):_)) = do
      -- FIXME: shouldn't have all this view-gnostic stuff..
      s <- atomically $ readTVar state
      -- Note: this means that if there has been an edit within the
      -- span, then the type info won't show for it.  Lets see if this
      -- is bothersome.
      forM (spanToRange s sid ss') $ \range -> do
        editor <- getEditor s sid
        removeOldTypeInfoMarker s sid editor
        -- Add new highlight marker.
        mid <- Ace.addMarker editor range "type-info-expr" "info" False
        -- Compute its positioning.
        (sx, _) <- Ace.getCharPosition editor (Ace.start range)
        (_, y) <- Ace.getCharPosition editor (Ace.end range)
        let multiline = Ace.row (Ace.start range) /= Ace.row (Ace.end range)
            x = if multiline then 12 else sx
        return (tys, x, y + 12, mid)

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

runSnippetCode :: TVar State -> SnippetId -> IO ()
runSnippetCode state sid = do
   editor <- readEditor state sid
   code <- Ace.getValue editor
   runCode state (BuildRequest sid [("main.hs", code)])

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
      -- TODO: Consider whether we want some other behavior when a
      -- query is requested for a non-current snippet.  Seems like we
      -- should let the user know why the query isn't being performed.
      Built sid' info | sid' == sid -> QueryRequested sid info query
      _ -> oldStatus

clearTypeInfo :: TVar State -> SnippetId -> IO ()
clearTypeInfo state sid = do
  s <- atomically $ readTVar state
  removeOldTypeInfoMarker s sid =<< getEditor s sid
  atomically $ modifyTVar state (ixSnippet sid . snippetTypeInfo .~ Nothing)

-- | Remove the old highlight marker if there is one.
removeOldTypeInfoMarker :: State -> SnippetId -> Ace.Editor -> IO ()
removeOldTypeInfoMarker s sid editor = do
  let moldInfo = s ^? ixSnippet sid . snippetTypeInfo . _Just
  case moldInfo of
    Nothing -> return ()
    Just (_, _, _, mid) -> Ace.removeMarker editor mid

-- | Sets the id-info which the haddock iframe should use for its url.
navigateDoc :: TVar State -> Maybe IdInfo -> IO ()
navigateDoc state minfo = do
  docs <- readUnmanagedOrFail state (^? stateDocs)
  setIFrameUrl docs (fromMaybe noDocsUrl (hackageLink =<< minfo))

hackageLink :: IdInfo -> Maybe Text
hackageLink (IdInfo IdProp{..} idScope) =
  if idScope == Binder || idScope == Local
    then Nothing
    else Just $
      "http://hackage.haskell.org/package/" <>
      packageName <>
      maybe "" ("-" <>) (fmap cleanPackageVersion packageVersion) <>
      "/docs/" <>
      dotToDash moduleName <>
      ".html#" <>
      haddockSpaceMarks idSpace <>
      ":" <>
      idName
  where
    ModuleId {..} = fromMaybe idDefinedIn idHomeModule
    PackageId {..} = modulePackage
    dotToDash = T.map (\c -> if c == '.' then '-' else c)

-- | Show approximately what Haddock adds to documentation URLs.
haddockSpaceMarks :: IdNameSpace -> Text
haddockSpaceMarks VarName   = "v"
haddockSpaceMarks DataName  = "v"
haddockSpaceMarks TvName    = "t"
haddockSpaceMarks TcClsName = "t"

-- | Switches which tab is currently focused.
switchTab :: TVar State -> Tab -> IO ()
switchTab state = setTVarIO state stateTab

-- | Reset the UI's status as if compilation hadn't yet happened.
closeControls :: TVar State -> IO ()
closeControls stateVar = setTVarIO stateVar stateStatus InitialStatus
