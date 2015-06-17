-- | This module defines how the Docs tab is rendered.
module View.Docs (docsTab) where

import qualified Data.Text as T
import           Import

-- TODO: use React.IFrame for this.

docsTab :: State -> React ()
docsTab state =
  case state ^. stateDocs of
    Nothing -> div_ $ do
      class_ "soh-no-docs-available"
      text "No docs available for selection"
    Just info -> build "iframe" $ src_ (hackageLink info)

hackageLink :: IdInfo -> Text
hackageLink (IdInfo IdProp{..} idScope) =
  if idScope == Binder || idScope == Local
    --FIXME: handle this more gracefully
    then "<local identifier>"
    else
      "http://hackage.haskell.org/package/" <>
      packageName <>
      maybe "" ("-" <>) (fmap cleanPackageVersion packageVersion) <>
      "/docs/" <>
      dotToDash moduleName <>
      ".html#" <>
      haddockSpaceMarks idSpace <>
      ":" <>
      idName
  where
    ModuleId {..} = fromMaybe idDefinedIn idHomeModule
    PackageId {..} = modulePackage
    dotToDash = T.map (\c -> if c == '.' then '-' else c)

-- | Show approximately what Haddock adds to documentation URLs.
haddockSpaceMarks :: IdNameSpace -> Text
haddockSpaceMarks VarName   = "v"
haddockSpaceMarks DataName  = "v"
haddockSpaceMarks TvName    = "t"
haddockSpaceMarks TcClsName = "t"
