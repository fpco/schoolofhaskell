module View.TypeInfo (typePopup) where

import           Control.Monad (void)
import qualified Data.Text as T
import           Import
import           Model (switchTab, navigateDoc)
import           React.Internal (internalLiftIOReact)
import           View.Annotation

-- | Show the type popup.
typePopup :: [ResponseAnnExpType] -> Int -> Int -> React ()
typePopup typs x y = div_ $ do
  class_ "type-popup"
  style "top" $ T.pack (show (y + 14)) <> "px"
  style "left" $ T.pack (show x) <> "px"
  forM_ typs $ \(ResponseAnnExpType typ _) -> do
    spans' <- internalLiftIOReact $
      getTypeHighlightSpans (mkNoNewlines (annText typ))
    div_ $ do
      class_ "type-info highlighted-haskell ace-tomorrow"
      div_ $ do
        class_ "ace_line"
        void $ renderAnn spans' typ renderTypeAnn

renderTypeAnn :: TypeAnn -> React a -> React a
renderTypeAnn (TypeIdInfo info) inner = span_ $ do
  class_ "docs-link"
  title_ (displayIdInfo info)
  onClick $ \_ state -> do
    navigateDoc state (Just info)
    switchTab state DocsTab
  inner
