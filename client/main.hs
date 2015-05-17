{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Import hiding (children)
import           Model
import qualified React.Ace as Ace
import           React.Internal
import qualified React.TermJs as TermJs
import           View

-- | Grab the container element used for rendering into and start the
-- rendering loop.
main :: IO ()
main = do
  app <- getApp
  ace <- Ace.new app
  termjs <- TermJs.new app
  _ <- forkIO $ runApp app
  -- Just for development
  -- _ <- forkIO $ do
  --   state <- readTVarIO (appState app)
  --   threadDelay (1000 * 1000)
  --   let editor = Ace.aceEditorOrError (state ^. stateAce)
  --   code <- Ace.getValue editor
  --   runCode (appState app) [("main.hs", code)]
  react app (render ace termjs) =<< getElementById "react-container"
