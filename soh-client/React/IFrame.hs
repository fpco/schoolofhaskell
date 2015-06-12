module React.IFrame
  ( IFrame(..)
  , buildIFrame
  , setIFrameUrl
  ) where

import Control.Lens (Traversal')
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (forM_)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHCJS.DOM.Element (Element, elementSetAttribute)
import GHCJS.DOM.Node (nodeAppendChild)
import GHCJS.Types (JSString)
import Import.Util (getElement)
import React.Internal (Component, ReactT)
import React.Unmanaged (Unmanaged, buildUnmanaged)
import GHCJS.Marshal (ToJSRef, FromJSRef)

newtype IFrame = IFrame Element
  deriving (Typeable,  ToJSRef, FromJSRef)

buildIFrame
  :: MonadIO m
  => Component state (Unmanaged IFrame) m
  -> Traversal' state (Unmanaged IFrame)
  -> Maybe Text
  -> ReactT state m ()
buildIFrame c l murl = buildUnmanaged c l $ \_ q -> do
  parent <- getElement 0 q
  el <- createIFrame
  forM_ murl $ elementSetAttribute el ("src" :: JSString)
  void $ nodeAppendChild parent (Just el)
  return (IFrame el)

setIFrameUrl :: IFrame -> Text -> IO ()
setIFrameUrl (IFrame el) = elementSetAttribute el ("src" :: JSString)

foreign import javascript unsafe "document.createElement('iframe')"
  createIFrame :: IO Element
