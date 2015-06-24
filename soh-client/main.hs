{-# LANGUAGE OverloadedStrings #-}

import ContainerClient
import Control.Concurrent
import Import
import Model
import SchoolOfHaskell.Scheduler.API
import View (renderControls, renderEditor)

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
    code <- getElementText el
    let renderer = renderEditor ace termjs iframe sid code inlineControls
    void $ forkIO $ react app renderer el
  -- Run the application
#if LOCAL_SOH_RUNNER
  -- soh-runner.sh maps port 3000 to 3001
  let devMappings = PortMappings [(4000, 4000), (3000, 3001)]
  runApp "localhost" devMappings devReceipt app
#else
  let spec = ContainerSpec "soh-runner"
  -- clearContainers
  receipt <- createContainer spec
  (host, ports) <- pollForContainerAddress 60 $
    getContainerDetailByReceipt receipt
  runApp host ports receipt app
#endif

-- clearContainers :: IO ()
-- clearContainers = do
--   containers <- listContainers
--   forM_ containers stopContainerById
