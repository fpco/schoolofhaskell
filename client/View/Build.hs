module View.Build
  ( runButton
  , setSnippetClass
  , buildStatusText
  , buildTab
  ) where

import           Import
import           Model (runCode)
import qualified React.Ace as Ace

runButton :: State -> React ()
runButton state = div_ $ do
  class_ "run glyphicon"
  title_ "Compile and run code"
  onClick $ \_ stateVar -> do
    let editor = Ace.aceEditorOrError (state ^. stateAce)
    code <- Ace.getValue editor
    runCode stateVar [("main.hs", code)]

setSnippetClass :: Maybe Status -> React ()
setSnippetClass mstatus = class_ $ "snippet " <>
  case mstatus of
    Nothing -> "never-built"
    Just BuildRequested {} -> "building"
    Just Building {} -> "building"
    Just Built {} -> "built"
    Just QueryRequested {} -> "built"

buildStatusText :: Status -> Text
buildStatusText (BuildRequested _) = "Sending"
buildStatusText (Building (Just progress)) =
  "Building (" <>
  tshow (progressStep progress) <>
  "/" <>
  tshow (progressNumSteps progress) <>
  ")"
buildStatusText (Building Nothing) = "Fetching"
buildStatusText (Built info) = infoStatusText info
buildStatusText (QueryRequested info _) = infoStatusText info

infoStatusText :: BuildInfo -> Text
infoStatusText BuildInfo {..}
  | not (null buildServerDieds) =
    "Server Died"
  | not (null buildErrors) =
    "Errors"
  | not (null buildWarnings) =
    "Warnings"
  | otherwise =
    "Built"

buildTab :: Status -> React ()
buildTab (BuildRequested _) = return ()
buildTab (Building (Just progress)) = forM_ (progressParsedMsg progress) text
buildTab (Building Nothing) = "Build done.  Requesting compile info.."
buildTab (Built info) = buildInfo info
buildTab (QueryRequested info _) = buildInfo info

buildInfo :: BuildInfo -> React ()
buildInfo info =
  forM_ (sourceErrors info) $ \err -> div_ $ do
    --FIXME: have some explanatory text or victory picture when there
    --are no errors or warnings.
    class_ $ "message " <> case errorKind err of
      KindError -> "kind-error"
      KindServerDied -> "kind-error"
      KindWarning -> "kind-warning"
    span_ $ do
      class_ "error-span"
      text $ tshow (errorSpan err)
    span_ $ do
      class_ "error-msg"
      text (errorMsg err)

sourceErrors :: BuildInfo -> [SourceError]
sourceErrors info =
  buildServerDieds info ++
  buildErrors info ++
  buildWarnings info
