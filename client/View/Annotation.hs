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
import           IdeSession.Client.JsonAPI.Common (sliceSpans)
import           Import hiding (ix, to)
import           Model (switchTab, navigateDoc)

--------------------------------------------------------------------------------
-- Annotations

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
getExpHighlightSpans :: NoNewlines -> IO ClassSpans
getExpHighlightSpans = getHighlightSpansWithPrefix $ mkNoNewlines "x = "

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

-- FIXME: should probably use source spans / allow new lines instead
-- of having this newtype...
newtype NoNewlines = NoNewlines Text
  deriving (Eq, Show, Monoid)

unNoNewlines :: NoNewlines -> Text
unNoNewlines (NoNewlines x) = x

mkNoNewlines :: Text -> NoNewlines
mkNoNewlines = fromMaybe (error "mkNoNewlines failed") . mayMkNoNewlines

mayMkNoNewlines :: Text -> Maybe NoNewlines
mayMkNoNewlines x | "\n" `T.isInfixOf` x = Nothing
mayMkNoNewlines x = Just (NoNewlines x)
