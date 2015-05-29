module View.Editor where

import qualified Ace
import Import
import PosMap (handleChange, selectionToSpan)
import Model (runQuery)

buildEditor
  :: UComponent Ace.Editor
  -> React ()
buildEditor ace = buildUnmanaged ace stateAce $ \stateVar q -> do
  editor <- Ace.makeEditor q
  Ace.setValue editor "main = (readLn :: IO Int) >>= print"
  Ace.onSelectionChange editor =<< debounce 100 (handleSelectionChange stateVar)
  Ace.onChange editor (handleChange stateVar)
  return editor

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
