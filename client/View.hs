module View where

import           Import
import           Model (runQuery, switchTab)
import qualified React.Ace as Ace
import           React.TermJs (TermJs)
import           View.Build
import           View.Console
import           View.Docs
import           View.TypeInfo

render :: Component Ace.Ace -> Component TermJs -> State -> React ()
render ace termjs state = div_ $ do
  let mstatus = state ^. stateStatus
  h1_ "SoH snippet demo"
  div_ $ do
    setSnippetClass mstatus
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
