-- | This module defines how the console tab is rendered.
module View.Console (consoleTab) where

import Model.Protocol (sendProcessInput)
import Data.Text (unpack)
import Import
import JavaScript.TermJs

consoleTab :: UComponent TermJs -> React ()
consoleTab termJs = do
  buildUnmanaged termJs stateConsole $ \state q -> do
    terminal <- initTerminal q
    onTerminalData terminal $ \input -> do
      mbackend <- viewTVarIO state stateBackend
      forM_ mbackend $ \backend ->
        sendProcessInput backend (unpack input)
    return terminal
