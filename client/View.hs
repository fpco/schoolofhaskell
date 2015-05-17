module View where

import           Communication (sendProcessInput)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types
import           Import
import           Model (runCode, runQuery, switchTab)
import qualified React.Ace as Ace
import           React.Builder (refAttr)
import qualified React.TermJs as TermJs
import           System.IO.Unsafe (unsafePerformIO)
import           View.TypeInfo

render :: Component Ace.Ace -> Component TermJs.TermJs -> State -> React ()
render ace termjs state = div_ $ do
  let mstatus = state ^. stateStatus
  h1_ "SoH snippet demo"
  div_ $ do
    class_ $ case mstatus of
      Nothing -> "snippet never-built"
      Just BuildRequested {} -> "snippet building"
      Just Building {} -> "snippet building"
      Just Built {} -> "snippet built"
      Just QueryRequested {} -> "snippet built"
    buildComponent ace stateAce $ do
      Ace.defaultValue_
        "main = (readLn :: IO Int) >>= print"
--        "import Control.Concurrent.Async (race)\n\nmain = putStrLn \"hello\" `race` putStrLn \"world\""
      Ace.onSelectionChange (const handleSelectionChange)
    case mstatus of
      Nothing -> runButton state
      Just status -> div_ $ do
        class_ "controls"
        div_ $ do
          class_ "controls-bar"
          runButton state
          mkTab state BuildTab $ text (buildStatusText status)
          mkTab state ConsoleTab "Console"
          mkTab state DocsTab "Docs"
        mkTabContent state BuildTab $ buildTab status
        mkTabContent state ConsoleTab $ consoleTab termjs
        mkTabContent state DocsTab $ docsTab state
    forM_ (state ^. stateTypes) $ \typs ->
      typePopup typs 300 100

runButton :: State -> React ()
runButton state = div_ $ do
  class_ "run glyphicon"
  title_ "Compile and run code"
  onClick $ \_ stateVar -> do
    let editor = Ace.aceEditorOrError (state ^. stateAce)
    code <- Ace.getValue editor
    runCode stateVar [("main.hs", code)]

buildStatusText :: Status -> Text
buildStatusText (BuildRequested _) = "Sending"
buildStatusText (Building (Just progress)) =
  "Building (" <>
  tshow (progressStep progress) <>
  "/" <>
  tshow (progressNumSteps progress) <>
  ")"
buildStatusText (Building Nothing) = "Fetching"
buildStatusText (Built info) = infoStatusText info
buildStatusText (QueryRequested info _) = infoStatusText info

infoStatusText :: BuildInfo -> Text
infoStatusText BuildInfo {..}
  | not (null buildServerDieds) =
    "Server Died"
  | not (null buildErrors) =
    "Errors"
  | not (null buildWarnings) =
    "Warnings"
  | otherwise =
    "Built"

buildTab :: Status -> React ()
buildTab (BuildRequested _) = return ()
buildTab (Building (Just progress)) = forM_ (progressParsedMsg progress) text
buildTab (Building Nothing) = "Build done.  Requesting compile info.."
buildTab (Built info) = buildInfo info
buildTab (QueryRequested info _) = buildInfo info

buildInfo :: BuildInfo -> React ()
buildInfo info =
  forM_ (sourceErrors info) $ \err -> div_ $ do
    --FIXME: have some explanatory text or victory picture when there
    --are no errors or warnings.
    class_ $ "message " <> case errorKind err of
      KindError -> "kind-error"
      KindServerDied -> "kind-error"
      KindWarning -> "kind-warning"
    span_ $ do
      class_ "error-span"
      text $ tshow (errorSpan err)
    span_ $ do
      class_ "error-msg"
      text (errorMsg err)

sourceErrors :: BuildInfo -> [SourceError]
sourceErrors info =
  buildServerDieds info ++
  buildErrors info ++
  buildWarnings info

consoleTab :: Component TermJs.TermJs -> React ()
consoleTab termJs =
  buildComponent termJs stateConsole $ do
    --TODO: weird that the code for handling stdin is in View and the
    --code for stdout is in Model...
    TermJs.onData $ \ev state -> do
      mbackend <- viewTVarIO state stateBackend
      forM_ mbackend $ \backend ->
        sendProcessInput backend (encodeUtf8 (TermJs.dataEventText ev))

docsTab :: State -> React ()
docsTab state =
  case state ^. stateDocs of
    Nothing -> span_ "FIXME: explanatory content"
    Just info -> build "iframe" $ src_ (hackageLink info)

hackageLink :: IdInfo -> Text
hackageLink (IdInfo IdProp{..} idScope) =
  if idScope == Binder || idScope == Local
    --FIXME: handle this more gracefully
    then "<local identifier>"
    else
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

mkTab :: State -> Tab -> React () -> React ()
mkTab state tab f = div_ $ do
  class_ $
    addWhen (state ^. stateTab == tab) "tab-focused"
    ("tab " <> tabClass tab)
  onClick (\_ -> flip switchTab tab)
  f

mkTabContent :: State -> Tab -> React () -> React ()
mkTabContent state tab f = div_ $ do
  class_ $
    addWhen (state ^. stateTab == tab) "tab-content-focused"
    ("tab-content " <> tabClass tab <> "-content")
  f

tabClass :: Tab -> Text
tabClass BuildTab = "build-tab"
tabClass ConsoleTab = "console-tab"
tabClass DocsTab = "docs-tab"

-- Queries

handleSelectionChange :: TVar State -> IO ()
handleSelectionChange state = do
  editor <- Ace.aceEditorOrError <$> viewTVarIO state stateAce
  mss <- fmap (aceSelectionToSourceSpan "main.hs") <$> Ace.getSelection editor
  forM_ mss $ runQuery state . QueryInfo

aceSelectionToSourceSpan :: FilePath -> Ace.Selection -> SourceSpan
aceSelectionToSourceSpan fp = aceRangeToSourceSpan fp . Ace.selectionToRange

aceRangeToSourceSpan :: FilePath -> Ace.Range -> SourceSpan
aceRangeToSourceSpan fp range = SourceSpan
  { spanFilePath   = fp
  , spanFromLine   = Ace.row    (Ace.start range) + 1
  , spanFromColumn = Ace.column (Ace.start range) + 1
  , spanToLine     = Ace.row    (Ace.end range)   + 1
  , spanToColumn   = Ace.column (Ace.end range)   + 1
  }
