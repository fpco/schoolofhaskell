module View.Build
  ( runButton
  , setSnippetClass
  , buildStatusText
  , buildTab
  ) where

import qualified Ace
import           Import
import           Model (runCode)
import           View.Annotation

runButton :: State -> React ()
runButton state = div_ $ do
  class_ "run glyphicon"
  title_ "Compile and run code"
  onClick $ \_ stateVar -> do
    editor <- getUnmanagedOrFail (state ^. stateAce)
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
  forM_ (sourceErrors info) $ \AnnSourceError{..} -> div_ $ do
    --FIXME: have some explanatory text or victory picture when there
    --are no errors or warnings.
    class_ $ "message " <> case annErrorKind of
      KindError -> "kind-error"
      KindServerDied -> "kind-error"
      KindWarning -> "kind-warning"
    span_ $ do
      text $ tshow annErrorSpan
      case annErrorSpan of
        TextSpan {} -> class_ "error-text-span"
        ProperSpan ss -> do
          class_ "error-proper-span"
          onClick $ \_ state -> do
            let (fp, sel) = Ace.spanToSelection ss
            editor <- readUnmanagedOrFail state (^. stateAce)
            Ace.setSelection editor sel
            Ace.focus editor
    span_ $ do
      class_ "error-msg"
      renderAnn [] annErrorMsg renderMsgAnn

renderMsgAnn :: MsgAnn -> React a -> React a
renderMsgAnn MsgAnnModule f = spanClass "msg-ann-module" f
renderMsgAnn MsgAnnCode f= spanClass "msg-ann-code" f
-- FIXME: add support for this
--   divClass "msg-ann-refactor" f
renderMsgAnn MsgAnnRefactor{} f = f
renderMsgAnn MsgAnnCollapse f = f
-- FIXME: add support for this
--   spanClass "msg-ann-collapse" $ return ()
--   span_ f

sourceErrors :: BuildInfo -> [AnnSourceError]
sourceErrors info =
  buildServerDieds info ++
  buildErrors info ++
  buildWarnings info
