-- | A React component which allows direct DOM construction and has a
-- single "init" property which specifies a callback for when the
-- component is mounted.  Particularly useful for using widgets
-- written in javascript which do not have react bindings, or when
-- react's model does not match well with the domain.
module JavaScript.Unmanaged
  ( Unmanaged(..)
  , getDefUnmanaged
  , newUnmanaged
  , buildUnmanaged
  , onInitUnmanaged
  , getUnmanaged
  , getUnmanagedOrFail
  , readUnmanaged
  , readUnmanagedOrFail
  ) where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ask)
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable, typeRep)
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import Import.Util (expectProp)
import JavaScript.JQuery (JQuery)
import Prelude
import React hiding (onClick)
import React.Builder (refAttr)
import React.Internal

newtype Unmanaged a = Unmanaged (JSRef a)

instance Show (Unmanaged a) where
  show _ = "<<Unmanaged>>"

instance Eq (Unmanaged a) where
  _ == _ = True

getDefUnmanaged :: IO (Unmanaged a)
getDefUnmanaged = return (Unmanaged jsNull)

newUnmanaged
  :: Monad m
  => App state m                          -- ^ The app.
  -> IO (Component state (Unmanaged a) m) -- ^ Component.
newUnmanaged app =
  createComponent
    (newClass app
              (return ())
              (didMount app)
              (\_ _ -> return ())
              (\_ _ -> return False)
              (\_ _ -> return ()))

didMount
  :: App state m
  -> Traversal' state (Unmanaged a)
  -> JQuery
  -> JSRef this
  -> IO ()
didMount app r el this = do
  props <- expectProp this "props"
  f <- getProp ("init" :: JSString) props
  x <- invokeCallback f el
  release f
  atomically $ modifyTVar (appState app) $ set r (Unmanaged x)

buildUnmanaged
  :: (MonadIO m, ToJSRef a)
  => Component state (Unmanaged a) m
  -> Traversal' state (Unmanaged a)
  -> (TVar state -> JQuery -> IO a)
  -> ReactT state m ()
buildUnmanaged c l f = buildComponent c l $ onInitUnmanaged f

onInitUnmanaged :: (MonadIO m, ToJSRef a) => (TVar state -> JQuery -> IO a) -> ReactT state m ()
onInitUnmanaged f = do
  app <- ask
  --FIXME: use retention based on the dom root.
  f' <- internalLiftIOReact $ syncCallback2 AlwaysRetain True $ \obj q -> do
    r <- toJSRef =<< f (appState app) q
    -- This is necessary because the callbacks can't directly return.
    setProp ("result" :: JSString) r obj
  refAttr "init" f'

getUnmanaged :: (Typeable a, FromJSRef a) => Unmanaged a -> IO (Maybe a)
getUnmanaged (Unmanaged ref) = fromJSRef ref

getUnmanagedOrFail :: forall a. (Typeable a, FromJSRef a) => Maybe (Unmanaged a) -> IO a
getUnmanagedOrFail munmanaged = do
  let shownType = "(Unmanaged (" ++ show (typeRep (Proxy :: Proxy a)) ++ "))"
  case munmanaged of
    Nothing -> fail $ "getUnmanagedOrFail received 'Nothing' for " ++ shownType
    Just unmanaged -> do
      mx <- getUnmanaged unmanaged
      case mx of
        Nothing -> fail $ "Failed to view " ++ shownType
        Just x -> return x

readUnmanaged
  :: (Typeable a, FromJSRef a) => TVar state -> (state -> Unmanaged a) -> IO (Maybe a)
readUnmanaged stateVar f = getUnmanaged . f =<< readTVarIO stateVar

readUnmanagedOrFail
  :: (Typeable a, FromJSRef a) => TVar state -> (state -> Maybe (Unmanaged a)) -> IO a
readUnmanagedOrFail stateVar f = getUnmanagedOrFail . f =<< readTVarIO stateVar

-- Better way to do this??

foreign import javascript unsafe "function() { var obj = {}; $1(obj, $2); return obj.result; }()"
  invokeCallback :: JSFun (JQuery -> IO ()) -> JQuery -> IO (JSRef a)
