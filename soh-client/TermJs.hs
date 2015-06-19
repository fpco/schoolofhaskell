-- | Bindings to the TermJs terminal emulator.
module TermJs
  ( TermJs(..)
  , initTerminal
  , writeTerminal
  , onTerminalData
  , onTerminalDisconnect
  ) where

import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHCJS.DOM.HTMLElement (HTMLElement)
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import Import.Util (getElement)
import JavaScript.JQuery (JQuery)

newtype TermJs = TermJs (JSRef TermJs)
  deriving (ToJSRef, FromJSRef, Typeable)

initTerminal :: JQuery -> IO TermJs
initTerminal q = do
  terminal <- newTerminal
  openTerminal terminal =<< getElement 0 q
  return terminal

writeTerminal :: TermJs -> Text -> IO ()
writeTerminal terminal = writeTerminal' terminal . toJSString

onTerminalData :: TermJs -> (Text -> IO ()) -> IO ()
onTerminalData terminal f = do
  parent <- toJSRef =<< terminalParent terminal
  f' <- syncCallback1 (DomRetain (coerce parent)) True (f . fromJSString)
  onTerminalData' terminal f'

onTerminalDisconnect :: TermJs -> IO () -> IO ()
onTerminalDisconnect terminal f = do
  parent <- toJSRef =<< terminalParent terminal
  f' <- syncCallback (DomRetain (coerce parent)) True f
  onTerminalDisconnect' terminal f'

--------------------------------------------------------------------------------
-- FFI

foreign import javascript
  "new Terminal({ useStyle: true, screenKeys: true })"
  newTerminal :: IO TermJs

foreign import javascript
  "$1.open($2)"
  openTerminal :: TermJs -> HTMLElement -> IO ()

-- foreign import javascript
--   "$1.destroy()"
--   destroyTerminal :: TermJs -> IO ()

-- foreign import javascript
--   "$1.focus()"
--   focusTerminal :: TermJs -> IO ()

foreign import javascript
  "$1.write($2)"
  writeTerminal' :: TermJs -> JSString -> IO ()

foreign import javascript
  "$1.on('data', $2)"
  onTerminalData' :: TermJs -> JSFun (JSString -> IO ()) -> IO ()

foreign import javascript
  "$1.on('disconnect', $2)"
  onTerminalDisconnect' :: TermJs -> JSFun (IO ()) -> IO ()

foreign import javascript
  "$1.parent"
  terminalParent :: TermJs -> IO HTMLElement
