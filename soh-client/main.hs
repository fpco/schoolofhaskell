{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Import hiding (getElementById)
import Model
import View (renderControls, renderEditor)
import GHCJS.DOM.HTMLElement (htmlElementGetInnerText, castToHTMLElement)
import qualified Ace

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
  -- Just for development purposes
  -- void $ forkIO $ initialRun (appState app)
  -- Run the application
  runApp app

initialRun :: TVar State -> IO ()
initialRun stateVar = do
  threadDelay (1000 * 1000)
  let sid = SnippetId 0
  code <- Ace.getValue =<< readEditor stateVar sid
  runCode stateVar (BuildRequest sid [("main.hs", code)])

getElementsByClassName :: JSString -> IO [Element]
getElementsByClassName name =
  mapM fromJSRefOrFail =<< fromArray =<< getElementsByClassName' name

getElementById :: JSString -> IO (Maybe Element)
getElementById name =
  fromJSRef =<< getElementById' name

foreign import javascript unsafe "document.getElementsByClassName($1)"
  getElementsByClassName' :: JSString -> IO (JSArray Element)

foreign import javascript unsafe "document.getElementById($1)"
  getElementById' :: JSString -> IO (JSRef Element)

foreign import javascript unsafe
  "positionControlsOnResize"
  positionControlsOnResize :: Element -> Element -> IO ()
-- about `-fdev`.
devMode :: Bool
devMode = fromJSBool devMode'

foreign import javascript unsafe
  "window['devMode']"
  devMode' :: JSBool
