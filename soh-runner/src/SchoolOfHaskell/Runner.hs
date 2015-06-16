{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module SchoolOfHaskell.Runner (Settings(..), runner) where

import           Control.Monad (void)
import           Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           IdeSession (defaultSessionInitParams, defaultSessionConfig)
import           IdeSession.Client (ClientIO(..), startEmptySession)
import           IdeSession.Client.CmdLine
import           IdeSession.Client.JsonAPI (toJSON, fromJSON)
import qualified Network.HTTP.Types as H
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import           SchoolOfHaskell.RunnerAPI
import           System.Timeout (timeout)

data Settings = Settings
  { settingsPort :: Int
  , settingsReceipt :: Text
  , settingsLifetime :: Maybe Int
  }

runner :: Settings -> IO ()
runner Settings {..} = do
  let -- Halts the server after a duration of time, if we have a
      -- lifetime limit.  This is a temporary solution to the problem
      -- of garbage collecting containers.
      lifetime
        | Just secs <- settingsLifetime =
          void . timeout (secs * 1000 * 1000)
        | otherwise = id
      warpSettings = Warp.setPort settingsPort Warp.defaultSettings
      app = runnerApp settingsReceipt
  lifetime $ Warp.runSettings warpSettings $ \req sendResponse -> sendResponse $
    case WaiWS.websocketsApp WS.defaultConnectionOptions app req of
      Just res -> res
      Nothing -> W.responseLBS H.status404
                               [ ("Content-Type", "text/plain") ]
                               "Not Found: expected a websockets connection"

runnerApp :: Text -> WS.PendingConnection -> IO ()
runnerApp receipt pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  let send = WS.sendTextData conn . encode . toJSON
      receive = do
        input <- WS.receiveData conn
        return $ fromJSON =<< eitherDecode input
  initial <- receive
  case initial of
    Right (RunnerRequestAuth receipt')
      | receipt' /= receipt ->
        send RunnerResponseAuthFailure
      | otherwise -> do
        send RunnerResponseAuthSuccess
        let sendResponse = send . RunnerResponseClient
            receiveRequest = do
              ereq <- receive
              return $ case ereq of
                Left err -> Left err
                Right (RunnerRequestClient x) -> Right x
                _ -> Left "Didn't expect runner request"
        startEmptySession ClientIO {..} clientOpts EmptyOptions
  where
    clientOpts = Options
      { optInitParams = defaultSessionInitParams
      , optConfig = defaultSessionConfig
      , optCommand = StartEmptySession EmptyOptions
      }
