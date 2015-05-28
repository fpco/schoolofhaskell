{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Import
import Model
import View

-- | Grab the container element used for rendering into and start the
-- rendering loop.
main :: IO ()
main = do
  app <- getApp
  ace <- newUnmanaged app
  termjs <- newUnmanaged app
  iframe <- newUnmanaged app
  _ <- forkIO $ runApp app
  -- Just for development
  -- _ <- forkIO $ do
  --   state <- readTVarIO (appState app)
  --   threadDelay (1000 * 1000)
  --   let editor = Ace.aceEditorOrError (state ^. stateAce)
  --   code <- Ace.getValue editor
  --   runCode (appState app) [("main.hs", code)]
  react app (render ace termjs iframe) =<< getElementById "react-container"
