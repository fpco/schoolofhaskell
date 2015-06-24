{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module SchoolOfHaskell.Runner (Settings(..), runner) where

import           Conduit (foldC, sourceHandle, ($$))
import           Control.Applicative ((<$>))
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, cancel)
import           Control.Exception (SomeException, catch, finally)
import           Control.Monad (void)
import           Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (forM_)
import           Data.IORef
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           IdeSession (defaultSessionInitParams, defaultSessionConfig)
import           IdeSession.Client (ClientIO(..), startEmptySession)
import           IdeSession.Client.CmdLine
import           IdeSession.Client.JsonAPI (toJSON, fromJSON)
import qualified Network.HTTP.Types as H
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import           Numeric (showHex)
import           SchoolOfHaskell.Runner.API
import           System.Directory (createDirectoryIfMissing)
import qualified System.IO as IO
import           System.Timeout (timeout)
import qualified GHC.IO.Handle as IO

data Settings = Settings
  { settingsPort :: Int
  , settingsReceipt :: Text
  , settingsLifetime :: Maybe Int
  }

runner :: Settings -> IO ()
runner Settings {..} = do
  logStdio settingsReceipt
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
  putStrLn $ "Accepting connection from client: " ++
    show (WS.pendingRequest pending)
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  putStrLn "Accepted connection and forked ping thread"
  let send = WS.sendTextData conn . encode . toJSON
      receive = do
        input <- WS.receiveData conn
        return $ fromJSON =<< eitherDecode input
  initial <- receive
  case initial of
    Right (RunnerRequestAuth receipt')
      | receipt' /= receipt -> do
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
        startEmptySession ClientIO {..} clientOpts EmptyOptions
          `finally` do
            mthread <- readIORef listenThreadRef
            forM_ mthread cancel
  where
    clientOpts = Options
      { optInitParams = defaultSessionInitParams
      , optConfig = defaultSessionConfig
      , optCommand = StartEmptySession EmptyOptions
      }

-- | Returns when some process is listening to the port.
waitForProcessListening :: Int -> IO ()
waitForProcessListening port = loop 120
  where
    loop :: Int -> IO ()
    loop gen = do
      isListening <- processListening port `catch` \e -> do
        --FIXME: some better error logging than this.
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

logStdio :: Text -> IO ()
logStdio receipt = do
  createDirectoryIfMissing False "logs"
  let fp = "logs/" ++ T.unpack receipt
  stdoutFile <- IO.openFile (fp ++ ".stdout") IO.WriteMode
  stderrFile <- IO.openFile (fp ++ ".stderr") IO.WriteMode
  IO.hDuplicateTo stdoutFile IO.stdout
  IO.hDuplicateTo stderrFile IO.stderr
  IO.hClose stdoutFile
  IO.hClose stderrFile
  IO.hSetBuffering IO.stdout IO.NoBuffering
  IO.hSetBuffering IO.stderr IO.NoBuffering
