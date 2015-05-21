module View where

import qualified Ace
import Import
import Model (runQuery, switchTab)
import TermJs
import View.Build
import View.Console
import View.Docs
import View.TypeInfo
import GHCJS.Marshal (toJSRef)

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
      Ace.onSelectionChange editor (handleSelectionChange stateVar)
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
  editor <- readUnmanagedOrFail state (^. stateAce)
  ss <- Ace.spanFromSelection  "main.hs" <$> Ace.getSelection editor
  runQuery state (QueryInfo ss)
