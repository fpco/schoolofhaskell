module SchoolOfHaskell.Runner (Settings(..), runner) where

import           Data.Aeson (encode, eitherDecode)
import           IdeSession (defaultSessionInitParams, defaultSessionConfig)
import           IdeSession.Client (ClientIO(..), startEmptySession)
import           IdeSession.Client.CmdLine
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

data Settings = Settings
  { settingsPort :: Int
  }

runner :: Settings -> IO ()
runner settings = do
  let warpSettings = Warp.defaultSettings
        { Warp.settingsPort = settingsPort settings
        }
      clientOpts = Options
        { optInitParams = defaultSessionInitParams
        , optConfig = defaultSessionConfig
        , optCommand = StartEmptySession EmptyOptions
        }
      -- TODO: fail more gracefully than this, possibly by changing
      -- ide-backend-client to have getJson :: IO (Maybe Value).
      decodeOrFail = either error id . eitherDecode
      app pending = do
        conn <- WS.acceptRequest pending
        WS.forkPingThread conn 30
        -- TODO: authenticate the container receipt
        -- msg <- WS.receiveData conn
        let clientIO = ClientIO
              { putJson = \x -> WS.sendTextData conn $ encode x
              , getJson = fmap decodeOrFail (WS.receiveData conn)
              }
        startEmptySession clientIO clientOpts EmptyOptions
  Warp.runSettings warpSettings $ \req sendResponse ->
    case WaiWS.websocketsApp WS.defaultConnectionOptions app req of
      Just res -> sendResponse res
      --TODO: better response than this?
      Nothing -> fail "Not a websockets connection"
