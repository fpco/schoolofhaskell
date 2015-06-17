{-# LANGUAGE OverloadedStrings #-}

import qualified Ace
import           ContainerClient
import           Control.Concurrent
import           GHCJS.DOM.HTMLElement (htmlElementGetInnerText, castToHTMLElement)
import           Import
import           Model
import           SchoolOfHaskell.Scheduler.API
import           View (renderControls, renderEditor)

-- | Main function of the School of Haskell client.
main :: IO ()
main = do
  -- Get the code elements.
  els <- getElementsByClassName "soh-code"
  -- Initialize app state
  app <- getApp (length els)
  ace <- newUnmanaged app
  termjs <- newUnmanaged app
  iframe <- newUnmanaged app
  -- Render the controls, if they're in a predefined div.
  mcontrols <- getElementById "soh-controls"
  mcolumn <- getElementById "soh-column"
  inlineControls <- case (mcontrols, mcolumn) of
    (Just controls, Just column) -> do
      void $ forkIO $ react app (renderControls termjs iframe) controls
      positionControlsOnResize controls column
      return False
    _ -> return True
  -- Substitute the code elements with editors
  forM_ (zip els [SnippetId 0..]) $ \(el, sid) -> do
    code <- htmlElementGetInnerText (castToHTMLElement el)
    let renderer = renderEditor ace termjs iframe sid code inlineControls
    void $ forkIO $ react app renderer el
  -- Run the application
  case mschedulerUrl of
    Nothing -> runApp "localhost" 4000 devReceipt app
    Just schedulerUrl -> do
      let spec = ContainerSpec "soh-runner"
          bu = BaseUrl schedulerUrl
      receipt <- createContainer bu spec
      (host, port) <-
        pollForContainerAddress 60 (getContainerDetailByReceipt bu receipt)
      runApp host port receipt app
