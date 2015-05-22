module View where

import qualified Ace
import Import
import Model (runQuery, switchTab)
import PosMap
import TermJs
import View.Build
import View.Console
import View.Docs
import View.TypeInfo

render
  :: Component (Unmanaged Ace.Editor)
  -> Component (Unmanaged TermJs)
  -> State
  -> React ()
render ace termjs state = div_ $ do
  let mstatus = state ^. stateStatus
  h1_ "SoH snippet demo"
  div_ $ do
    setSnippetClass mstatus
    buildComponent ace stateAce $ onInitUnmanaged $ \stateVar q -> do
      editor <- Ace.makeEditor q
      Ace.setValue editor "main = (readLn :: IO Int) >>= print"
      Ace.onSelectionChange editor =<< debounce 100 (handleSelectionChange stateVar)
      Ace.onChange editor (handleChange stateVar)
      return editor
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

handleSelectionChange :: TVar State -> IO ()
handleSelectionChange state = do
  -- Clear the old type info.
  setTVarIO state stateTypes Nothing
  -- Compute the source span of the query at the time of compilation.
  s <- readTVarIO state
  selection <- Ace.getSelection =<< getUnmanagedOrFail (s ^. stateAce)
  case selectionToSpan s selection of
    -- FIXME: UI for this.
    Nothing -> putStrLn "No span for this query"
    Just ss -> runQuery state (QueryInfo ss)
