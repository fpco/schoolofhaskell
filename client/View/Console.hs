module View.Console (consoleTab) where

import Communication (sendProcessInput)
import Data.Text.Encoding (encodeUtf8)
import Import
import Model (runQuery, switchTab)
import qualified React.TermJs as TermJs

consoleTab :: Component TermJs.TermJs -> React ()
consoleTab termJs =
  buildComponent termJs stateConsole $ do
    --TODO: weird that the code for handling stdin is in View and the
    --code for stdout is in Model...
    TermJs.onData $ \ev state -> do
      mbackend <- viewTVarIO state stateBackend
      forM_ mbackend $ \backend ->
        sendProcessInput backend (encodeUtf8 (TermJs.dataEventText ev))
