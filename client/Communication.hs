-- | Wrapper around JavaScript.WebSockets which has safely interruptible
-- sending and receiving of messages.
--
-- Originally attempted with vanilla Chan, but I ran into what I
-- believe to be an erroneous "thread blocked indefinitely in an MVar
-- operation".
--
-- This may be the issue documented here:
-- https://github.com/ghcjs/ghcjs/issues/320
--
-- (I am using a slighty older GHCJS, and don't have time to test HEAD)
module Communication where

import           Control.Concurrent.Async (race)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TChan
import           Control.Monad (forever)
import           Data.Text (Text)
import           Data.Void (absurd)
import qualified JavaScript.WebSockets as WS

data Connection = Connection
  { receiveTChan :: TChan Text
  , sendTChan :: TChan Text
  , connection :: WS.Connection
  }

-- NOTE: if the websocket disconnects, the inner function is killed.
-- This is due to using the unsafe functions in JavaScript.WebSockets
withUrl :: Text -> (Connection -> IO a) -> IO a
withUrl url f = do
    receiveTChan <- newTChanIO
    sendTChan <- newTChanIO
    result <- WS.withUrl url $ \connection ->
      receiveThread connection receiveTChan `race`
      sendThread connection sendTChan `race`
      f Connection {..}
    case result of
      Left (Left x) -> absurd x
      Left (Right x) -> absurd x
      Right x -> return x
  where
    receiveThread conn chan =
      forever $ WS.receiveText_ conn >>= atomically . writeTChan chan
    sendThread conn chan =
      forever $ atomically (readTChan chan) >>= WS.sendText_ conn

receiveText :: Connection -> IO Text
receiveText = atomically . readTChan . receiveTChan

sendText :: Connection -> Text -> IO ()
sendText conn = atomically . writeTChan (sendTChan conn)
