-- | This module defines how the console tab is rendered.
module View.Console (consoleTab) where

import Communication (sendProcessInput)
import Data.Text.Encoding (encodeUtf8)
import Import
import TermJs

consoleTab :: UComponent TermJs -> React ()
consoleTab termJs = do
  buildUnmanaged termJs stateConsole $ \state q -> do
    terminal <- initTerminal q
    onTerminalData terminal $ \input -> do
      mbackend <- viewTVarIO state stateBackend
      forM_ mbackend $ \backend ->
        sendProcessInput backend (encodeUtf8 input)
    return terminal
