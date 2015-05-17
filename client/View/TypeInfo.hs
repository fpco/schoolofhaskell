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
import           Import
import           Model (runCode, runQuery, switchTab, navigateDoc)
import qualified React.Ace as Ace
import           React.Builder (dangerouslySetInnerHTML)
import           React.Event
import           React.Internal (internalLiftIOReact)
import qualified React.TermJs as TermJs

-- | Show the type popup.
typePopup :: [ResponseAnnExpType] -> Int -> Int -> React ()
typePopup typs x y = div_ $ do
  class_ "type-popup"
  style "top" $ T.pack (show (y + 14)) <> "px"
  style "left" $ T.pack (show x) <> "px"
  allAnns <- fmap concat $ forM (zip [0..] typs) $ \(ix, ResponseAnnExpType anns typ _) -> do
    let namedAnns =
          [ (T.pack ("docs-link-" ++ show ix ++ "-" ++ show jx), ann)
          | jx <- [0..]
          | ann <- anns
          ]
        toMark (cls, AnnSpan fr to (TypeIdInfo info)) =
          (fr + off, to + off, "docs-link " <> cls, [("title", displayIdInfo info)])
        initialText = "x :: "
        off = T.length initialText
        className = "type-info highlighted-haskell ace-tomorrow"
    highlightHaskell (initialText <> typ) className $
      (0, off, "hide-mark", []) :
      map toMark namedAnns
    return namedAnns
  onClick $ \ev state -> do
    t <- target ev
    classes <- T.words <$> elementGetAttribute t ("class" :: JSString)
    forM_ (find ("docs-link-" `T.isPrefixOf`) classes) $ \cls -> do
      forM_ (find (\(cls', _) -> cls == cls') allAnns) $ \(_, AnnSpan _ _ (TypeIdInfo info)) -> do
        navigateDoc state (Just info)
        switchTab state DocsTab

highlightHaskell :: Text -> Text -> [(Int, Int, Text, [(Text, Text)])] -> React ()
highlightHaskell = highlightCode "ace/mode/haskell"

-- | Use Ace to highlight static code.  Uses the theme and mode of the
-- passed Editor.
highlightCode :: Text -> Text -> Text -> [(Int, Int, Text, [(Text, Text)])] -> React ()
highlightCode mode input className marks = div_ $ do
  class_ className
  html <- internalLiftIOReact $ do
    highlighted <- highlightCodeInternal
      (toJSString input)
      (toJSString mode)
      (toJSString className)
    -- While it'd be more efficient to do something clever like
    -- regexing on the html string, this is also more likely to be
    -- vulnerable to injection vulnerabilities.  (we're already
    -- relying on ace being free of such injection vulnerabilities..)
    sliceInnerText marks highlighted
  dangerouslySetInnerHTML html

-- | Annotates the inner text with classes by introducing additional
-- <span>s.
--
-- TODO: modify existing nodes when the span bounds match? (which is
-- frequently / always for this usecase)
--
-- NOTE: Class names should be unique.  Nested spans with the same
-- class name will cause an error to be thrown.
sliceInnerText :: [(Int, Int, Text, [(Text, Text)])] -> JSString -> IO JSString
sliceInnerText ivls input = do
  starts <- forM ivls $ \(fr, _, cls, props) -> do
    obj <- newObj
    forM_ props (\(k, v) -> setProp k (toJSString v) obj)
    return ((fr, False), obj, cls)
  let ends = map (\(_, to, cls, _) -> ((to, True), jsNull, cls)) ivls
      events =
        map (\((ix, _), obj, cls) -> (ix, obj, cls)) $
        sortBy (comparing (\(pos, _, _) -> pos)) $
        starts ++ ends
  container <- divFromInnerHTML input
  void $ join $ sliceInnerText'
    <$> toJSRef events
    <*> return container
  getInnerHTML container

foreign import javascript "highlightCode($1, $2, $3)"
  highlightCodeInternal :: JSString -> JSString -> JSString -> IO JSString

foreign import javascript "function() { var node = document.createElement('div'); node.innerHTML = $1; return node; }()"
  divFromInnerHTML :: JSString -> IO (JSRef Element)

foreign import javascript "$1.innerHTML"
  getInnerHTML :: JSRef Element -> IO JSString

foreign import javascript "sliceInnerText($1, $2)"
  sliceInnerText' :: JSRef [(Int, JSObject obj, Text)] -> JSRef Element -> IO ()
