module Ace
  ( Editor
    -- * Construction
  , makeEditor
    -- * Queries
  , getValue
  , getSelection
    -- * Mutations
  , setValue
  , setSelection
    -- * Events
  , onChange
  , onSelectionChange
    -- * Auxiliary Types
  , Pos(..)
  , Range(..)
  , Selection(..)
  , selectionToRange
  ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (void, join)
import           Data.Coerce (coerce)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types
import           Import.Util (getElement, intToJSNumber)
import qualified JavaScript.AceAjax.Raw.Ace as Ace
import qualified JavaScript.AceAjax.Raw.Editor as Editor
import qualified JavaScript.AceAjax.Raw.IEditSession as Session
import qualified JavaScript.AceAjax.Raw.Selection as Selection
import           JavaScript.AceAjax.Raw.Types hiding (Selection, Range)
import qualified JavaScript.AceAjax.Raw.VirtualRenderer as Renderer
import           JavaScript.JQuery (JQuery)

--------------------------------------------------------------------------------
-- Construction

makeEditor :: JQuery -> IO Editor
makeEditor q = do
  e <- join $ Ace.edit1 <$> ace <*> getElement 0 q
  Editor.setHighlightActiveLine e (toJSBool False)
  s <- Editor.session e
  Session.setUseWorker s (toJSBool False)
  Session.setMode s "ace/mode/haskell"
  r <- Editor.renderer e
  Renderer.setTheme r "ace/theme/tomorrow"
  Renderer.setShowGutter r (toJSBool False)
  Renderer.setShowPrintMargin r (toJSBool False)
  return e

--------------------------------------------------------------------------------
-- Queries

getValue :: FromJSString t => Editor -> IO t
getValue = fmap fromJSString . Editor.getValue

getSelection :: Editor -> IO Selection
getSelection editor = do
  s <- Editor.getSelection editor
  ma <- fromJSRef . coerce =<< Selection.getSelectionAnchor s
  ml <- fromJSRef . coerce =<< Selection.getSelectionLead s
  case (ma, ml) of
    (Just a, Just l) -> return Selection { anchor = a, lead = l }
    _ -> fail "Failed to marshal ace editor selection"

--------------------------------------------------------------------------------
-- Mutations

-- Note: -1 specifies that the cursor should be moved to the end
--
setValue :: Editor -> Text -> IO ()
setValue e val = void $ Editor.setValue e (toJSString val) (intToJSNumber (-1))

setSelection :: Editor -> Selection -> IO ()
setSelection editor Selection{..} = do
  s <- Editor.getSelection editor
  Selection.setSelectionAnchor s
    (intToJSNumber (row anchor))
    (intToJSNumber (column anchor))
  Selection.selectTo s
    (intToJSNumber (row lead))
    (intToJSNumber (column lead))

--------------------------------------------------------------------------------
-- Event Handlers

onChange :: Editor -> IO () -> IO ()
onChange e f = do
  -- TODO: open pull request on ace typescript bindings making things subtypes of EventEmitter?
  parent <- toJSRef =<< Editor.container e
  f' <- syncCallback (DomRetain (coerce parent)) True f
  editorOn e "change" f'

onSelectionChange :: Editor -> IO () -> IO ()
onSelectionChange e f = do
  -- TODO: open pull request on ace typescript bindings making things subtypes of EventEmitter?
  parent <- toJSRef =<< Editor.container e
  f' <- syncCallback1 (DomRetain (coerce parent)) True (\_ -> f >> return jsNull)
  s <- Editor.getSelection e
  Selection.on s "changeSelection" f'

--------------------------------------------------------------------------------
-- Positions and Ranges

data Pos = Pos
    { row :: Int
    , column :: Int
    }
    deriving (Show, Eq, Ord, Generic)

data Range = Range
    { start :: Pos
    , end :: Pos
    }
    deriving (Show, Eq, Generic)

data Selection = Selection
    { anchor :: Pos
    , lead :: Pos
    }
    deriving (Show, Eq, Generic)

instance ToJSRef   Pos       where toJSRef   = toJSRef_generic   id
instance FromJSRef Pos       where fromJSRef = fromJSRef_generic id
instance ToJSRef   Range     where toJSRef   = toJSRef_generic   id
instance FromJSRef Range     where fromJSRef = fromJSRef_generic id
instance ToJSRef   Selection where toJSRef   = toJSRef_generic   id
instance FromJSRef Selection where fromJSRef = fromJSRef_generic id

selectionToRange :: Selection -> Range
selectionToRange sel =
  if anchor sel < lead sel
    then Range (anchor sel) (lead sel)
    else Range (lead sel) (anchor sel)

--------------------------------------------------------------------------------
-- FFI

-- Needed due to incompleteness ghcjs-from-typescript / the typescript parser
foreign import javascript unsafe "function() { return ace; }()"
  ace :: IO Ace

-- Needed due to incompleteness of typescript ace definitions
foreign import javascript unsafe "$1.on($2, $3)"
  editorOn :: Editor -> JSString -> JSFun (IO ()) -> IO ()
