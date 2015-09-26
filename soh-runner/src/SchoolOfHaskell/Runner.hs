{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module SchoolOfHaskell.Runner (Settings(..), runner) where

import           Conduit (foldC, sourceHandle, ($$))
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, cancel)
import           Control.Exception (SomeException, AsyncException(ThreadKilled), catch, finally, fromException)
import           Control.Monad (void, when)
import           Data.Aeson (encode, eitherDecode)
import           Data.Foldable (forM_)
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           IdeSession (defaultSessionInitParams, sessionConfigFromEnv, SessionConfig(..))
import           Stack.Ide
import           Stack.Ide.CmdLine
import qualified Network.HTTP.Types as H
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import           Numeric (showHex)
import           SchoolOfHaskell.Runner.API
import qualified System.IO as IO
import           System.Timeout (timeout)

data Settings = Settings
  { settingsPort :: Int
  , settingsReceipt :: Text
  , settingsLifetime :: Maybe Int
  , settingsVerbose :: Bool
  }

runner :: Settings -> IO ()
runner settings@Settings {..} = do
  sessionConfig' <- sessionConfigFromEnv
  let sessionConfig = sessionConfig' { configLocalWorkingDir = Nothing }
      -- Halts the server after a duration of time, if we have a
      -- lifetime limit.  This is a temporary solution to the problem
      -- of garbage collecting containers.
      lifetime
        | Just secs <- settingsLifetime =
          void . timeout (secs * 1000 * 1000)
        | otherwise = id
      warpSettings = Warp.setPort settingsPort Warp.defaultSettings
      app = runnerApp settings sessionConfig
  lifetime $ Warp.runSettings warpSettings $ \req sendResponse -> sendResponse $
    case WaiWS.websocketsApp WS.defaultConnectionOptions app req of
      Just res -> res
      Nothing -> W.responseLBS H.status404
                               [ ("Content-Type", "text/plain") ]
                               "Not Found: expected a websockets connection"

runnerApp :: Settings -> SessionConfig -> WS.PendingConnection -> IO ()
runnerApp Settings{..} sessionConfig pending = do
  putStrLn $ "Accepting connection from client: " ++
    show (WS.pendingRequest pending)
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  putStrLn "Accepted connection and forked ping thread"
  let send = WS.sendTextData conn . encode
      receive = do
        input <- WS.receiveData conn
        return $ eitherDecode input
  initial <- receive
  case initial of
    Right (RunnerRequestAuth receipt')
      | receipt' /= settingsReceipt -> do
        putStrLn "Authentication failed"
        send RunnerResponseAuthFailure
      | otherwise -> do
        putStrLn "Authentication succeeded"
        send RunnerResponseAuthSuccess
        listenThreadRef <- newIORef Nothing
        let sendResponse = send . RunnerResponseClient
            receiveRequest = do
              ereq <- receive
              case ereq of
                Left err -> return $ Left err
                Right (RunnerRequestClient x) -> return $ Right x
                Right (RunnerRequestPortListening port) -> do
                  thread <- async $ do
                    waitForProcessListening port
                    send RunnerResponsePortIsListening
                  mold <- atomicModifyIORef listenThreadRef (Just thread, )
                  forM_ mold cancel
                  receiveRequest
                Right req -> return $ Left $
                  "Didn't expect runner request: " ++ show req
            logMessage loc source level str =
              when settingsVerbose $ sendLog clientIO loc source level str
            clientIO = ClientIO {..}
            clientOpts = Options
              { optInitParams = defaultSessionInitParams
              , optConfig = sessionConfig
              , optVerbose = False
              , optVersion = False
              }
        sendExceptions clientIO $ startEmptySession clientIO clientOpts
          `finally` do
            mthread <- readIORef listenThreadRef
            forM_ mthread cancel
    _ -> send RunnerResponseAuthFailure

-- | Returns when some process is listening to the port.
waitForProcessListening :: Int -> IO ()
waitForProcessListening port = loop 120
  where
    loop :: Int -> IO ()
    loop gen = do
      isListening <- processListening port `catch` \e -> do
        case e of
          (fromException -> Just ThreadKilled) -> return False
          _ -> do
            -- FIXME: some better error logging than this.
            putStrLn $ "Exception while listening for port: " ++
              show (e :: SomeException)
            return False
      if isListening
        then return ()
        else do
          threadDelay $ if gen <= 0 then 5000000 else 250000
          loop (max 0 (gen - 1))

processListening :: Int -> IO Bool
processListening port =
    any findPort . T.lines . decodeUtf8 <$>
    localReadFile "/proc/net/tcp"
  where
    -- Fun fact: files in the /proc filesystem will often report their
    -- size as 0. Because of this, the standard Data.ByteString.readFile
    -- will read in an empty ByteString. Instead, we use conduit here.
    localReadFile fp =
      IO.withBinaryFile fp IO.ReadMode $ \h ->
      sourceHandle h $$ foldC
    goalPortHex =
      T.toUpper $ T.takeEnd 4 $ T.pack ("0000" ++ showHex port "")
    findPort line =
      case T.words line of
        (_sl:(T.stripPrefix ":" . T.dropWhile (/= ':') -> Just portHex):_) ->
          portHex == goalPortHex
        _ -> False
