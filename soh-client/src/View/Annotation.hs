-- | This module provides utilities for rendering the 'Ann' type of the
-- API. 'Ann' gives extra structure to textual information provided by
-- the backend, by adding nested annotations atop the text.
--
-- In the current School of Haskell code, 'Ann' is used for source
-- errors and type info. This allows things like links to docs for
-- identifiers, and better styling for source errors.
--
-- This module also provides utilities for getting highlight spans from
-- code, via Ace. This allows the display of annotated info to highlight
-- the involved expressions / types, and pass these 'ClassSpans' into
-- 'renderAnn'.
module View.Annotation
  ( -- * Annotations
    renderAnn
  , annText
    -- * Rendering IdInfo links
  , renderCodeAnn
    -- * Highlighting Code
  , getHighlightSpans
  , getExpHighlightSpans
  , getTypeHighlightSpans
    -- ** Utilities
  , NoNewlines
  , unNoNewlines
  , mkNoNewlines
  , mayMkNoNewlines
  ) where

import qualified Data.Text as T
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types
import           Import hiding (ix, to)
import           Model (switchTab, navigateDoc)

--------------------------------------------------------------------------------
-- Annotations

-- | This renders an 'Ann' type, given a function for rendering the
-- annotations.
--
-- This rendering function takes the annotation, and is given the
-- 'React' rendering of the nested content.  This allows it to add
-- parent DOM nodes / attributes, in order to apply the effect of the
-- annotation.
--
-- It also takes a 'ClassSpans' value, which is used at the leaf
-- level, to slice up the spans of text, adding additional class
-- annotations.  This is used to add the results of code highlighting
-- to annotated info.
renderAnn
  :: forall a.
     ClassSpans
  -> Ann a
  -> (forall b. a -> React b -> React b)
  -> React ()
renderAnn spans0 x0 f = void $ go 0 spans0 x0
  where
    go :: Int -> ClassSpans -> Ann a -> React (Int, ClassSpans)
    go ix spans (Ann ann inner) = f ann $ go ix spans inner
    go ix spans (AnnGroup []) = return (ix, spans)
    go ix spans (AnnGroup (x:xs)) = do
      (ix', spans') <- go ix spans x
      go ix' spans' (AnnGroup xs)
    go ix spans (AnnLeaf txt) = do
        forM_ (sliceSpans ix txt spans) $ \(chunk, mclass) -> span_ $ do
          forM_ mclass class_
          text chunk
        return (end, dropWhile (\(_, end', _) -> end' <= end) spans)
      where
        end = ix + T.length txt

annText :: Ann a -> Text
annText (Ann _ x) = annText x
annText (AnnGroup xs) = T.concat (map annText xs)
annText (AnnLeaf x) = x

--------------------------------------------------------------------------------
-- Rendering IdInfo links

-- | Renders a 'CodeAnn'.  This function is intended to be passed in
-- to 'renderAnn', or used to implement a function which is passed
-- into it.
renderCodeAnn :: CodeAnn -> React a -> React a
renderCodeAnn (CodeIdInfo info) inner = span_ $ do
  class_ "docs-link"
  title_ (displayIdInfo info)
  onClick $ \_ state -> do
    navigateDoc state (Just info)
    switchTab state DocsTab
  inner

--------------------------------------------------------------------------------
-- Highlighting code

type ClassSpans = [(Int, Int, Text)]

-- NOTE: prefixing for expressions doesn't seem to make a difference
-- for the current highlighter, but it might in the future.

-- | Get the highlight spans of an expression.
getExpHighlightSpans :: NoNewlines -> IO ClassSpans
getExpHighlightSpans = getHighlightSpansWithPrefix $ mkNoNewlines "x = "

-- | Get the highlight spans of a type.
getTypeHighlightSpans :: NoNewlines -> IO ClassSpans
getTypeHighlightSpans = getHighlightSpansWithPrefix $ mkNoNewlines "x :: "

getHighlightSpansWithPrefix :: NoNewlines -> NoNewlines -> IO ClassSpans
getHighlightSpansWithPrefix prefix codeLine = do
  let offset = T.length (unNoNewlines prefix)
  spans <- getHighlightSpans "ace/mode/haskell" (prefix <> codeLine)
  return $
    dropWhile (\(_, to, _) -> to <= 0) $
    map (\(fr, to, x) -> (fr - offset, to - offset, x)) spans

getHighlightSpans :: Text -> NoNewlines -> IO ClassSpans
getHighlightSpans mode (NoNewlines codeLine) =
  highlightCodeHTML (toJSString mode) (toJSString codeLine) >>=
  indexArray 0 >>=
  fromJSRef >>=
  maybe (fail "Failed to access highlighted line html") return >>=
  divFromInnerHTML >>=
  spanContainerToSpans >>=
  fromJSRef >>=
  maybe (fail "Failed to marshal highlight spans") return

foreign import javascript "function() { var node = document.createElement('div'); node.innerHTML = $1; return node; }()"
  divFromInnerHTML :: JSString -> IO (JSRef Element)

foreign import javascript "highlightCodeHTML"
  highlightCodeHTML :: JSString -> JSString -> IO (JSArray JSString)

foreign import javascript "spanContainerToSpans"
  spanContainerToSpans :: JSRef Element -> IO (JSRef ClassSpans)

--------------------------------------------------------------------------------
-- NoNewlines: utility for code highlighting

-- TODO: should probably use source spans / allow new lines instead
-- of having this newtype...

-- | This newtype enforces the invariant that the stored 'Text' doesn't
-- have the character \"\\n\".
newtype NoNewlines = NoNewlines Text
  deriving (Eq, Show, Monoid)

unNoNewlines :: NoNewlines -> Text
unNoNewlines (NoNewlines x) = x

mkNoNewlines :: Text -> NoNewlines
mkNoNewlines = fromMaybe (error "mkNoNewlines failed") . mayMkNoNewlines

mayMkNoNewlines :: Text -> Maybe NoNewlines
mayMkNoNewlines x | "\n" `T.isInfixOf` x = Nothing
mayMkNoNewlines x = Just (NoNewlines x)
