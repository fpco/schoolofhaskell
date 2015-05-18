module View.TypeInfo (typePopup) where

import           Communication (sendProcessInput)
import           Control.Monad (void, join)
import           Data.List (sortBy, find)
import           Data.Ord (comparing)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           GHCJS.DOM.Element (Element, elementGetAttribute)
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types
import           IdeSession.Client.JsonAPI.Common (sliceSpans)
import           Import
import           Model (runCode, runQuery, switchTab, navigateDoc)
import qualified React.Ace as Ace
import           React.Builder (dangerouslySetInnerHTML)
import           React.Event
import           React.Internal (internalLiftIOReact)
import qualified React.TermJs as TermJs

-- Feature wishlist:
--
-- * Explanations for builtin type syntax.  For example, explanations
-- of forall, (=>), tuples, lists, etc.  These could also be made into
-- links to relevant documentation, though that might bug advanced
-- users (could make this a setting).
--
-- * Highlighting of AST nodes of the type on hover.  This lets the
-- user know the structure of the type's AST, and can aid in learning
-- Haskell syntax.  Unfortunately, this is not possible to do
-- perfectly with our current information, because we need fixities.
-- Even for advanced users, this will be helpful for understanding
-- types that involve infix operators.
--
-- * Allow type synonyms to be replaced by definitions and vice versa.
--
-- I think the best implementation strategy for these wishlist items
-- is to expose annotated ASTs from ide-backend, possibly involving
-- modifications of GHC itself.  Until then, we'll stick to the
-- haskell-src-exts / autocomplete map annotation of type info.

-- | Show the type popup.
typePopup :: [ResponseAnnExpType] -> Int -> Int -> React ()
typePopup typs x y = div_ $ do
  class_ "type-popup"
  style "top" $ T.pack (show (y + 14)) <> "px"
  style "left" $ T.pack (show x) <> "px"
  forM_ (zip [0..] typs) $ \(ix, ResponseAnnExpType typ _) -> do
    let txt = typeText typ
        prefix = "x :: "
        offset = T.length prefix
    spans <- internalLiftIOReact $
      getHighlightSpans "ace/mode/haskell" (prefix <> txt)
    let spans' =
          dropWhile (\(_, to, _) -> to <= 0) $
          map (\(fr, to, x) -> (fr - offset, to - offset, x)) spans
    internalLiftIOReact $ consoleLog =<< toJSRef spans'
    div_ $ do
      class_ "type-info highlighted-haskell ace-tomorrow"
      div_ $ do
        class_ "ace-line"
        void $ renderAnn 0 spans' typ renderTypeAnn

renderTypeAnn :: TypeAnn -> React a -> React a
renderTypeAnn (TypeIdInfo info) inner = span_ $ do
  class_ "docs-link"
  title_ (displayIdInfo info)
  onClick $ \_ state -> do
    navigateDoc state (Just info)
    switchTab state DocsTab
  inner

renderAnn
  :: Int
  -> HighlightSpans
  -> Ann a
  -> (forall b. a -> React b -> React b)
  -> React (Int, HighlightSpans)
renderAnn ix spans (Ann ann inner) f =
  f ann $ renderAnn ix spans inner f
renderAnn ix0 spans0 (AnnGroup xs0) f =
    go ix0 spans0 xs0
  where
    go ix spans [] = return (ix, spans)
    go ix spans (x:xs) = do
      (ix', spans') <- renderAnn ix spans x f
      go ix' spans' xs
renderAnn ix spans (AnnLeaf txt) f = do
    forM_ (sliceSpans ix txt spans) $ \(chunk, mclass) -> span_ $ do
      forM_ mclass class_
      text chunk
    return (end, dropWhile (\(_, end', _) -> end' <= end) spans)
  where
    end = ix + T.length txt

typeText :: Ann TypeAnn -> Text
typeText (Ann _ x) = typeText x
typeText (AnnGroup xs) = T.concat (map typeText xs)
typeText (AnnLeaf x) = x

--NOTE: assumes one line of input.
getHighlightSpans :: Text -> Text -> IO HighlightSpans
getHighlightSpans mode codeLine =
  highlightCodeHTML (toJSString mode) (toJSString codeLine) >>=
  indexArray 0 >>=
  fromJSRef >>=
  maybe (fail "Failed to access highlighted line html") return >>=
  divFromInnerHTML >>=
  spanContainerToSpans >>=
  fromJSRef >>=
  maybe (fail "Failed to marshal highlight spans") return

type HighlightSpans = [(Int, Int, Text)]

foreign import javascript "function() { var node = document.createElement('div'); node.innerHTML = $1; return node; }()"
  divFromInnerHTML :: JSString -> IO (JSRef Element)

foreign import javascript "highlightCodeHTML"
  highlightCodeHTML :: JSString -> JSString -> IO (JSArray JSString)

foreign import javascript "spanContainerToSpans"
  spanContainerToSpans :: JSRef Element -> IO (JSRef HighlightSpans)
