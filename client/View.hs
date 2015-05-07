module View where

import GHCJS.Types (JSRef)
import Import
import JavaScript.JQuery (JQuery)
import Model (runCode, runQuery, switchTab)
import qualified React.Ace as Ace
import Control.Monad.Trans (lift)
import React.Internal (internalLiftIOReact)
import GHCJS.Foreign (fromJSString)

render :: Component Ace.Ace -> State -> React ()
render ace state = div_ $ do
  let mstatus = state ^. stateStatus
  h1_ (text "SoH snippet demo")
  div_ $ do
    class_ $ case mstatus of
      Nothing -> "snippet never-built"
      Just BuildRequested {} -> "snippet building"
      Just Building {} -> "snippet building"
      Just Built {} -> "snippet built"
      Just QueryRequested {} -> "snippet built"
    buildComponent ace stateAce $ do
      Ace.defaultValue_ "main = putStrLn \"world\""
      Ace.onSelectionChange (const handleSelectionChange)
    case mstatus of
      Nothing -> runButton state
      Just status -> div_ $ do
        class_ "controls"
        runButton state
        div_ $ do
          class_ "controls-bar"
          mkTab state BuildTab $ text (buildStatusText status)
          mkTab state ConsoleTab "Console"
          mkTab state InfoTab "Info"
        case state ^. stateTab of
          BuildTab -> buildTab status
          ConsoleTab -> consoleTab state
          InfoTab -> infoTab state

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
buildTab status = div_ $ do
  class_ "tab-content build-tab-content"
  case status of
    BuildRequested _ -> return ()
    Building (Just progress) ->
      forM_ (progressParsedMsg progress) text
    Building Nothing ->
      text "Build done.  Requesting compile info.."
    Built info -> buildInfo info
    QueryRequested info _ -> buildInfo info

buildInfo :: BuildInfo -> React ()
buildInfo info =
  forM_ (sourceErrors info) $ \err -> div_ $ do
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

consoleTab :: State -> React ()
consoleTab state = div_ $ do
  class_ "tab-content console-tab-content"
  mapM_ (span_ . text) (state ^. stateConsole)

infoTab :: State -> React ()
infoTab state = div_ $ do
  class_ "tab-content docs-tab-content"
  span_ $ text $ state ^. stateInfo

mkTab :: State -> Tab -> React () -> React ()
mkTab state tab f = div_ $ do
  class_ $
    addWhen (state ^. stateTab == tab) "tab-focused"
    ("tab " <> tabClass tab)
  onClick (\_ state -> switchTab state tab)
  f

tabClass :: Tab -> Text
tabClass BuildTab = "build-tab"
tabClass ConsoleTab = "console-tab"
tabClass InfoTab = "info-tab"

-- Queries

handleSelectionChange :: TVar State -> IO ()
handleSelectionChange state = do
  editor <- Ace.aceEditorOrError <$> viewTVarIO stateAce state
  mss <- fmap (aceSelectionToSourceSpan "main.hs") <$> Ace.getSelection editor
  tab <- viewTVarIO stateTab state
  case (tab, mss) of
    (InfoTab, Just ss) -> do
      print ("running query", ss)
      runQuery state (QueryInfo ss)
    _ -> return ()

aceSelectionToSourceSpan :: FilePath -> Ace.Selection -> SourceSpan
aceSelectionToSourceSpan fp = aceRangeToSourceSpan fp . Ace.selectionToRange

aceRangeToSourceSpan :: FilePath -> Ace.Range -> SourceSpan
aceRangeToSourceSpan fp range = SourceSpan
  { spanFilePath = fp
  , spanFromLine = Ace.row (Ace.start range) + 1
  , spanFromColumn = Ace.column (Ace.start range) + 1
  , spanToLine = Ace.row (Ace.end range) + 1
  , spanToColumn = Ace.column (Ace.end range) + 1
  }
