{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | React binding to Ace.
module React.Ace
  ( -- * Type
    Ace(..)
  , Editor_
  , Editor'
  , getDef
    -- * Construction
  , new
    -- * Properties
  , defaultValue_
  , selection_
    -- * Events
  , ChangeEvent(..)
  , onChange
  , SelectionEvent(..)
  , onSelectionChange
    -- * Positions and Ranges
  , Pos(..)
  , Range(..)
  , Selection(..)
    -- * Queries
  , aceEditorOrError
  , getValue
  , getSelection
  , selectionToRange
    -- * Mutations
  , setValue
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.STM
import Control.Lens
import Control.Monad (join, when, (>=>))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Foldable (forM_)
import Data.Text (Text)
import GHCJS.Foreign
import GHCJS.Types
import Import.Util (prop, mtprop, expectProp, setPropShow)
import JavaScript.JQuery (JQuery)
import React hiding (onClick)
import React.Internal

--------------------------------------------------------------------------------
-- Types

-- | Ace component's state.
data Ace = Ace (Maybe Editor')

-- | An Editor editor.
data Editor_
type Editor' = JSRef Editor_

-- | Default state for instances of ace.
getDef :: IO Ace
getDef = return (Ace Nothing)

--------------------------------------------------------------------------------
-- Component

-- | Create an Ace editor component.
new :: Monad m
    => App state m                -- ^ The app.
    -> IO (Component state Ace m) -- ^ Ace component.
new app =
  createComponent
    (newClass app
              (return ())
              (didMount app)
              (\_l _props -> return ())
              (\_ _ -> return False)
              (receivingProps app))

-- | Setup the ace editor.
didMount :: App a m -> Traversal' a Ace -> JQuery -> JSRef this -> IO ()
didMount app r el this =
  do props <- expectProp this "props"
     editor <- join $ makeEditor el
       <$> getProp ("defaultValue" :: JSString) props
       <*> getProp ("onChange" :: JSString) props
       <*> getProp ("onSelectionChange" :: JSString) props
     atomically
       (modifyTVar (appState app)
                   (set r (Ace (Just editor))))

-- | New value attribute has been set, update the editor contents.
receivingProps :: App state m -> Traversal' state Ace -> JSRef a -> IO ()
receivingProps app l props =
  do meditor <- fmap (preview l)
                     (atomically (readTVar (appState app)))
     case meditor of
       Nothing -> return ()
       Just (Ace Nothing) -> return ()
       Just (Ace (Just editor)) ->
         do mcode <- prop props "code"
            print (fmap (fromJSString) mcode :: Maybe String)
            forM_ mcode $ \code ->
              do code' <- getValueRef editor
                 when (not (code `stringEq` code'))
                      (setValueRef editor code)
            mnewSel <- getSelectionFromObject props
            moldSel <- getSelection editor
            forM_ mnewSel $ \newSel -> when (Just newSel /= moldSel) $
              setRange editor (row (anchor newSel))
                              (column (anchor newSel))
                              (row (lead newSel))
                              (column (lead newSel))

--------------------------------------------------------------------------------
-- Properties

defaultValue_ :: Monad m => Text -> ReactT state m ()
defaultValue_ = attr "defaultValue"

selection_ :: Monad m => Selection -> ReactT state m ()
selection_ sel = do
  setPropShow "anchor-row" (row (anchor sel))
  setPropShow "anchor-column" (column (anchor sel))
  setPropShow "anchor-row" (row (lead sel))
  setPropShow "anchor-column" (column (lead sel))

--------------------------------------------------------------------------------
-- Component events

newtype ChangeEvent = ChangeEvent ReactEvent
instance IsReactEvent ChangeEvent

onChange :: Monad m => (ChangeEvent -> TVar state -> IO ()) -> ReactT state m ()
onChange = onEvent (EventListener "change")

newtype SelectionEvent = SelectionEvent ReactEvent
instance IsReactEvent SelectionEvent

onSelectionChange :: Monad m => (SelectionEvent -> TVar state -> IO ()) -> ReactT state m ()
onSelectionChange = onEvent (EventListener "selectionChange")

--------------------------------------------------------------------------------
-- Positions and Ranges

data Pos = Pos
    { row :: Int
    , column :: Int
    }
    deriving (Show, Eq, Ord)

data Range = Range
    { start :: Pos
    , end :: Pos
    }
    deriving (Show, Eq)

data Selection = Selection
    { anchor :: Pos
    , lead :: Pos
    }
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Editor state queries

aceEditorOrError :: Ace -> Editor'
aceEditorOrError (Ace Nothing) = error "getEditorOrFail: No editor."
aceEditorOrError (Ace (Just x)) = x

getSelection :: Editor' -> IO (Maybe Selection)
getSelection = getSelectionRef >=> getSelectionFromObject

getSelectionFromObject :: JSRef a -> IO (Maybe Selection)
getSelectionFromObject obj = runMaybeT $ Selection
  <$> (Pos <$> mtprop obj "anchor-row"
           <*> mtprop obj "anchor-column")
  <*> (Pos <$> mtprop obj "lead-row"
           <*> mtprop obj "lead-column")

selectionToRange :: Selection -> Range
selectionToRange sel =
  if anchor sel < lead sel
    then Range (anchor sel) (lead sel)
    else Range (lead sel) (anchor sel)

getValue :: Editor' -> IO Text
getValue e = fromJSString <$> getValueRef e

setValue :: Editor' -> Text -> IO ()
setValue e = setValueRef e . toJSString

--------------------------------------------------------------------------------
-- Foreign imports

foreign import javascript "makeEditor($1,$2,$3,$4)"
  makeEditor :: JQuery
             -> JSString
             -> JSFun (JSRef props0 -> IO ())
             -> JSFun (JSRef props1 -> IO ())
             -> IO Editor'

foreign import javascript "($1).setValue($2,-1)"
  setValueRef :: Editor' -> JSString -> IO ()

foreign import javascript "getSelection($1)"
  getSelectionRef :: Editor' -> IO (JSRef Selection)

foreign import javascript "($1).selection.setSelectionRange(new AceRange($2,$3,$4,$5))"
  setRange :: Editor' -> Int -> Int -> Int -> Int -> IO ()

foreign import javascript "($1).getValue()"
  getValueRef :: Editor' -> IO JSString

foreign import javascript "$1===$2"
  stringEq :: JSString -> JSString -> Bool

--------------------------------------------------------------------------------
-- Instances

instance Show Ace where
    show (Ace m) = "<Ace " ++ c ++ " <MVar ()>>"
      where c = case m of
                  Nothing -> "Nothing"
                  Just _ -> "Just <Editor>"

instance Eq Ace where
    Ace Nothing == Ace Nothing = True
    Ace (Just{}) == Ace (Just {}) = True
    _ == _ = False
