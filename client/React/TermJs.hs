module React.TermJs
  ( TermJs(..)
  , TermJs'
  , getDef
  , new
  -- * Events
  , DataEvent
  , onData
  , dataEventText
  -- * commands
  , writeTerminal
  ) where

import Control.Concurrent.STM
import Control.Lens hiding (coerce)
import Data.Coerce (coerce)
import Data.Foldable (forM_)
import Data.Text (Text)
import GHCJS.Foreign
import GHCJS.Types
import Import.Util (prop, expectProp)
import JavaScript.JQuery (JQuery)
import React
import React.Internal

newtype TermJs = TermJs TermJs'

data TermJs_
type TermJs' = JSRef TermJs_

getDef :: IO TermJs
getDef = do
  terminal <- newTerminal
  return (TermJs terminal)

new :: Monad m
    => App state m
    -> IO (Component state TermJs m)
new app =
  createComponent
    (newClass app
              (return ())
              (didMount app)
              (\_ _ -> return ())
              (\_ _ -> return False)
              (\_ _ -> return ()))

didMount :: App a m -> Traversal' a TermJs -> JQuery -> JSRef this -> IO ()
didMount app r el this = do
  props <- expectProp this "props"
  monData <- prop props "onData"
  state <- atomically $ readTVar (appState app)
  case state ^? r of
    Nothing -> fail "didMount: No TermJs"
    Just (TermJs terminal) -> do
      openTerminal terminal el
      forM_ monData $ onTerminalData terminal

--------------------------------------------------------------------------------
-- Componenet events

newtype DataEvent = DataEvent ReactEvent
instance IsReactEvent DataEvent

onData :: Monad m => (DataEvent -> TVar state -> IO ()) -> ReactT state m ()
onData = onEvent (EventListener "data")

dataEventText :: DataEvent -> Text
dataEventText = fromJSString . coerce

--------------------------------------------------------------------------------
-- FFI

foreign import javascript
  "new Terminal({ useStyle: true, screenKeys: true })"
  newTerminal :: IO TermJs'

foreign import javascript
  "$1.open($2.get(0))"
  openTerminal :: TermJs' -> JQuery -> IO ()

--TODO: determine what this should be used for.
foreign import javascript
  "$1.destroy()"
  destroyTerminal :: TermJs' -> IO ()

foreign import javascript
  "$1.focus()"
  focusTerminal :: TermJs' -> IO ()

foreign import javascript
  "$1.write($2)"
  writeTerminal :: TermJs' -> JSString -> IO ()

foreign import javascript
  "$1.on('data', $2)"
  onTerminalData :: TermJs' -> JSFun (JSString -> IO ()) -> IO ()

foreign import javascript
  "$1.on('disconnect', $2)"
  onTerminalDisconnect :: TermJs' -> JSFun (IO ()) -> IO ()


--------------------------------------------------------------------------------
-- Instances

instance Show TermJs where
    show (TermJs _) = "<TermJs>"

instance Eq TermJs where
    _ == _ = True
