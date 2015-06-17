-- | This module defines how the build tab is rendered.
module View.Build
  ( buildStatusText
  , buildStatusClass
  , buildTab
  ) where

import qualified Ace
import qualified Data.Text as T
import           Import
import           Model (runCode)
import           PosMap (spanToSelection)
import           View.Annotation

-- TODO: bring this back
-- ghciButton :: React ()
-- ghciButton = div_ $ do
--   class_ "ghci-button btn btn-default"
--   text "GHCI"
--   onClick $ \_ state -> do
--     editor <- readUnmanagedOrFail state (^. stateAce)
--     code <- Ace.getValue editor
--     --FIXME: when using GHCI, don't display build errors / messages
--     --from it.
--     --FIXME: it'd also be good if ide-backend-client held on to the
--     --old session, so that info is still available.
--     let code' = T.unlines
--           [ "import System.Process (rawSystem)"
--           , "main = do"
--           , "  writeFile \"main.hs\" " <> tshow code
--           , "  ec <- rawSystem \"ghci\" [\"main.hs\"]"
--           , "  putStrLn $ \"GHCI exited with \" ++ show ec"
--           ]
--     runCode state [("main.hs", code')]

buildStatusText :: Status -> Text
buildStatusText InitialStatus = "Unbuilt"
buildStatusText (BuildRequested _) = "Sending"
buildStatusText (Building _ (Just progress)) =
  "Building (" <>
  tshow (progressStep progress) <>
  "/" <>
  tshow (progressNumSteps progress) <>
  ")"
buildStatusText (Building _ Nothing) = "Fetching"
buildStatusText (Built _ info) = infoStatusText info
buildStatusText (QueryRequested _ info _) = infoStatusText info
buildStatusText (KillRequested _ _) = "Killing"

-- TODO: use these classes to add style to the build tab.
buildStatusClass :: Status -> Text
buildStatusClass InitialStatus {} = ""
buildStatusClass BuildRequested {} = "soh-build-requested"
buildStatusClass Building {} = "soh-building"
buildStatusClass Built {} = "soh-built"
buildStatusClass QueryRequested {} = "soh-query-requested"
buildStatusClass KillRequested {} = "soh-kill-requested"

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
buildTab InitialStatus = return ()
buildTab (BuildRequested _) = return ()
buildTab (Building _ (Just progress)) = forM_ (progressParsedMsg progress) text
buildTab (Building _ Nothing) = "Build done.  Requesting compile info.."
buildTab (Built sid info) = buildInfo sid info
buildTab (QueryRequested sid info _) = buildInfo sid info
buildTab (KillRequested sid info) = buildInfo sid info

buildInfo :: SnippetId -> BuildInfo -> React ()
buildInfo sid info
  -- TODO: victory picture instead of text?
  | null (sourceErrors info) =
  text "Successful build - no errors or warnings!"
  | otherwise =
  forM_ (sourceErrors info) $ \AnnSourceError{..} -> div_ $ do
    -- FIXME: have some explanatory text or victory picture when there
    -- are no errors or warnings.
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
          onClick $ \_ stateVar -> do
            -- FIXME: use a more lenient version of spanToSelection
            -- which allows for removals within the span.  Possibly
            -- have the implementation function always compute the
            -- lenient version, but flag that some portion of the
            -- range was edited.
            --
            -- If the above isn't done, then at least the UI
            -- should mention why it isn't selecting anything on
            -- click, in this case.
            state <- readTVarIO stateVar
            editor <- getEditor state sid
            case spanToSelection state sid ss of
              Nothing -> putStrLn "No span for error"
              Just sel -> do
                Ace.setSelection editor sel
                Ace.focus editor
    span_ $ do
      class_ "error-msg"
      renderAnn [] annErrorMsg renderMsgAnn

renderMsgAnn :: MsgAnn -> React a -> React a
renderMsgAnn MsgAnnModule f = spanClass "msg-ann-module" f
renderMsgAnn (MsgAnnCode _) f = spanClass "msg-ann-code" f
renderMsgAnn (MsgAnnCodeAnn x) f = renderCodeAnn x f
-- TODO: add support for this
--   divClass "msg-ann-refactor" f
renderMsgAnn MsgAnnRefactor{} f = f
renderMsgAnn MsgAnnCollapse f = f
-- TODO: add support for this
--   spanClass "msg-ann-collapse" $ return ()
--   span_ f

sourceErrors :: BuildInfo -> [AnnSourceError]
sourceErrors info =
  buildServerDieds info ++
  buildErrors info ++
  buildWarnings info
