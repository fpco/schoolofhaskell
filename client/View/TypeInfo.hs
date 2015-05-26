module View.TypeInfo (typePopup) where

import qualified Data.Text as T
import           Import
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
        void $ renderAnn spans' typ renderCodeAnn
