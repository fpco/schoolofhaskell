{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module SchoolOfHaskell.Runner (Settings(..), runner) where

import           Control.Monad (void)
import           Data.Aeson (encode, eitherDecode)
import           Data.Text (Text)
import           IdeSession (defaultSessionInitParams, defaultSessionConfig)
import           IdeSession.Client (ClientIO(..), startEmptySession)
import           IdeSession.Client.CmdLine
import qualified Network.HTTP.Types as H
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import           System.Timeout (timeout)

data Settings = Settings
  { settingsPort :: Int
  , settingsReceipt :: Text
  , settingsLifetime :: Maybe Int
  }

runner :: Settings -> IO ()
runner Settings {..} = do
  let warpSettings = Warp.setPort settingsPort Warp.defaultSettings
      clientOpts = Options
        { optInitParams = defaultSessionInitParams
        , optConfig = defaultSessionConfig
        , optCommand = StartEmptySession EmptyOptions
        }
      -- Times out the server if we have a lifetime limit.  This is a
      -- temporary solution to the problem of garbage collecting
      -- containers.
      lifetime
        | Just secs <- settingsLifetime =
          void . timeout (secs * 1000 * 1000)
        | otherwise = id
      -- TODO: fail more gracefully than this, possibly by changing
      -- ide-backend-client to have getJson :: IO (Maybe Value).
      decodeOrFail = either error id . eitherDecode
      app pending = do
        conn <- WS.acceptRequest pending
        WS.forkPingThread conn 30
        -- TODO: authenticate the container receipt
        clientReceipt <- WS.receiveData conn
        if clientReceipt /= settingsReceipt
          then WS.sendTextData conn ("Error: Incorrect container receipt" :: Text)
          else do
            WS.sendTextData conn ("Success: Correct container receipt" :: Text)
            let clientIO = ClientIO
                  { putJson = \x -> WS.sendTextData conn $ encode x
                  , getJson = fmap decodeOrFail (WS.receiveData conn)
                  }
            startEmptySession clientIO clientOpts EmptyOptions
  lifetime $ Warp.runSettings warpSettings $ \req sendResponse -> sendResponse $
    case WaiWS.websocketsApp WS.defaultConnectionOptions app req of
      Just res -> res
      Nothing -> W.responseLBS H.status404
                               [ ("Content-Type", "text/plain") ]
                               "Not Found: expected a websockets connection"
