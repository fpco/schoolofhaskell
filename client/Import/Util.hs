module Import.Util where

import Control.Concurrent.STM
import Control.Exception (SomeException, catch, throwIO)
import Control.Lens
import Control.Monad (unless)
import Data.IORef
import Data.Monoid
import Data.Text (Text, pack)
import GHCJS.Foreign (toJSString)
import GHCJS.Prim (JSRef)
import IdeSession.Client.JsonAPI (Response)

addWhen :: Bool -> Text -> Text -> Text
addWhen True x y = y <> " " <> x
addWhen False _ y = y

tshow :: Show a => a -> Text
tshow = pack . show

once :: IO () -> IO (IO ())
once runAction = do
  alreadyCalledRef <- newIORef False
  return $ do
    alreadyCalled <- readIORef alreadyCalledRef
    unless alreadyCalled runAction

--------------------------------------------------------------------------------
-- Tvar/lens helpers

setTVarIO :: TVar s -> ASetter' s a -> a -> IO ()
setTVarIO v l a =
  atomically
    (modifyTVar v
                (set l a))

modifyTVarIO :: TVar s -> ASetter' s a -> (a -> a) -> IO ()
modifyTVarIO v l f =
  atomically
    (modifyTVar v
                (over l f))

-- waitForTVarIO :: TVar s -> APrism' s a -> IO a
waitForTVarIO v f = atomically $ do
  x <- readTVar v
  case f x of
    Just y -> return y
    Nothing -> retry

-- viewTVarIO :: Getting a s a -> TVar s -> IO a
viewTVarIO g v =
  atomically
    (fmap (view g)
          (readTVar v))

foreign import javascript "console.log($1)" consoleLog :: JSRef a -> IO ()

foreign import javascript "console.error($1)" consoleError :: JSRef a -> IO ()

showExceptions :: Text -> IO a -> IO a
showExceptions msg f = f `catch` \ex -> do
  consoleError $ toJSString (msg <> tshow (ex :: SomeException))
  throwIO ex

$(makePrisms ''Response)
