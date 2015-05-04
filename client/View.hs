{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module View where

import GHCJS.Types (JSRef)
import Import
import JavaScript.JQuery (JQuery)
import Model (runCode, switchTab)
import qualified React.Ace as Ace
import Control.Monad.Trans (lift)
import React.Internal (internalLiftIOReact)
import GHCJS.Foreign (fromJSString)

render :: Component Ace.Ace -> State -> React ()
render ace state = div_ $ do
  let mstatus = state ^. stateStatus
  h1_ (text "SoH snippet demo")
  div_ $ do
    class_ $ case mstatus of
      Nothing -> "snippet never-built"
      Just (BuildRequested _) -> "snippet building"
      Just (Building _) -> "snippet building"
      Just (Built _) -> "snippet built"
    buildComponent ace stateAce $ do
      Ace.defaultValue_ "main = putStrLn \"world\""
    case mstatus of
      Nothing -> runButton state
      Just status -> div_ $ do
        class_ "controls"
        runButton state
        div_ $ do
          class_ "controls-bar"
          mkTab state BuildTab $ text (buildStatusText status)
          mkTab state ConsoleTab "Console"
          mkTab state DocsTab "Docs"
        case state ^. stateTab of
          BuildTab -> buildTab status
          ConsoleTab -> consoleTab state
          DocsTab -> return ()

runButton :: State -> React ()
runButton state = div_ $ do
  class_ "run glyphicon"
  title_ "Compile and run code"
  onClick $ \_ stateVar -> do
    let editor = Ace.aceEditorOrError (state ^. stateAce)
    code <- Ace.getValue editor
    runCode [("main.hs", code)] stateVar

buildStatusText :: BuildStatus -> Text
buildStatusText (BuildRequested _) = "Sending"
buildStatusText (Building (Just progress)) =
  "Building (" <>
  tshow (progressStep progress) <>
  "/" <>
  tshow (progressNumSteps progress) <>
  ")"
buildStatusText (Building Nothing) =
  "Fetching"
buildStatusText (Built info)
  | not (null (buildServerDieds info)) =
    "Server Died"
  | not (null (buildErrors info)) =
    "Errors"
  | not (null (buildWarnings info)) =
    "Warnings"
  | otherwise =
    "Built"

buildTab :: BuildStatus -> React ()
buildTab status = div_ $ do
  class_ "tab-content build-tab-content"
  case status of
    BuildRequested _ -> return ()
    Building (Just progress) ->
      forM_ (progressParsedMsg progress) text
    Building Nothing ->
      text "Build done.  Requesting compile info.."
    Built info ->
      forM_ (sourceErrors info) $ \err -> div_ $ do
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

consoleTab :: State -> React ()
consoleTab state = div_ $ do
  class_ "tab-content console-tab-content"
  mapM_ (span_ . text) (state ^. stateConsole)

mkTab :: State -> Tab -> React () -> React ()
mkTab state tab f = div_ $ do
  class_ $
    addWhen (state ^. stateTab == tab) "tab-focused"
    ("tab " <> tabClass tab)
  onClick (const (switchTab tab))
  f

tabClass :: Tab -> Text
tabClass BuildTab = "build-tab"
tabClass ConsoleTab = "console-tab"
tabClass DocsTab = "docs-tab"
