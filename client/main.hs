{-# LANGUAGE OverloadedStrings #-}

import           Model
import           View


import           Control.Concurrent
import           Import hiding (children)
import           JavaScript.JQuery (select, children, remove)
import qualified React.Ace as Ace
import           React.Internal (App(appState))

-- | Grab the container element used for rendering into and start the
-- rendering loop.
main :: IO ()
main = do
    app <- getApp
    ace <- Ace.new app
    _ <- forkIO $ runApp app
    -- Just for development
    -- _ <- forkIO $ do
    --   threadDelay (10 * 1000)
    --   runCode [("main.hs", "main = putStrLn 1")] (appState app)
    react app (render ace) =<< getElementById "react-container"
