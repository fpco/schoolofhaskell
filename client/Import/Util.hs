module Import.Util where

import           Control.Concurrent.STM
import           Control.Exception (SomeException, catch, throwIO)
import           Control.Lens hiding (coerce)
import           Control.Monad (unless)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Char (isHexDigit)
import           Data.Coerce (coerce)
import           Data.Foldable (forM_)
import           Data.IORef
import           Data.Monoid
import           Data.Text (Text, pack)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           GHCJS.DOM.HTMLElement (HTMLElement)
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types
import           IdeSession.Client.JsonAPI
import           IdeSession.Types.Public
import           JavaScript.JQuery (JQuery)
import           React
import           React.Lucid
import           System.IO.Unsafe (unsafePerformIO)

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

debounce :: Int -> IO () -> IO (IO ())
debounce ms f = do
  f' <- asyncCallback AlwaysRetain f
  mtimeoutRef <- newIORef Nothing
  return $ do
    mtimeout <- readIORef mtimeoutRef
    forM_ mtimeout clearTimeout
    writeIORef mtimeoutRef . Just =<< setTimeout f' ms

newtype TimeoutId = TimeoutId (JSRef TimeoutId)
  deriving (Typeable, ToJSRef, FromJSRef)

foreign import javascript unsafe "setTimeout($1, $2)"
  setTimeout :: JSFun (IO ()) -> Int -> IO TimeoutId

foreign import javascript unsafe "clearTimeout($1)"
  clearTimeout :: TimeoutId -> IO ()

--------------------------------------------------------------------------------
-- Misc utils

prop :: FromJSRef a => JSRef obj -> JSString -> IO (Maybe a)
prop obj n = do
  ref <- getProp n obj
  if isUndefined ref || isNull ref
    then return Nothing
    else fromJSRef ref

mtprop :: FromJSRef a => JSRef obj -> JSString -> MaybeT IO a
mtprop obj n = MaybeT $ prop obj n

expectProp :: FromJSRef a => JSRef obj -> JSString -> IO a
expectProp obj n = do
  mx <- prop obj n
  case mx of
    Nothing -> fail $ "Couldn't find expected property " ++ fromJSString n
    Just x -> return x

setPropShow :: (Monad m, Show a) => T.Text -> a -> ReactT state m ()
setPropShow n = attr n . T.pack . show

divClass :: Monad m => Text -> ReactT state m a -> ReactT state m a
divClass className f = div_ $ do
  class_ className
  f

spanClass :: Monad m => Text -> ReactT state m a -> ReactT state m a
spanClass className f = span_ $ do
  class_ className
  f

foreign import javascript unsafe "$2.get($1)"
  getElement :: Int -> JQuery -> IO HTMLElement

-- FIXME: Find a better way to do this, or add similar utilities to ghcjs-base
intToJSNumber :: Int -> JSNumber
intToJSNumber n = coerce $ unsafePerformIO $ toJSRef n

fromJSRefOrFail :: FromJSRef a => String -> JSRef a -> IO a
fromJSRefOrFail what ref = do
  mx <- fromJSRef ref
  case mx of
    Nothing -> fail $ "Failed to marshal " ++ what
    Just x -> return x

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

waitForTVarIO :: TVar s -> (s -> Maybe a) -> IO a
waitForTVarIO v f = atomically $ do
  x <- readTVar v
  case f x of
    Just y -> return y
    Nothing -> retry

viewTVarIO ::  TVar s -> Getting a s a -> IO a
viewTVarIO v g =
  atomically
    (fmap (view g)
          (readTVar v))

foreign import javascript "console.log($1)" consoleLog :: JSRef a -> IO ()

foreign import javascript "console.warn($1)" consoleWarn :: JSRef a -> IO ()

foreign import javascript "console.error($1)" consoleError :: JSRef a -> IO ()

showExceptions :: Text -> IO a -> IO a
showExceptions msg f = f `catch` \ex -> do
  consoleError $ toJSString ("Exception in " <> msg <> ": " <> tshow (ex :: SomeException))
  throwIO ex

showAndIgnoreExceptions :: Text -> IO () -> IO ()
showAndIgnoreExceptions msg f = f `catch` \ex ->
  consoleError $ toJSString ("Exception ignored in " <> msg <> ": " <> tshow (ex :: SomeException))

--------------------------------------------------------------------------------
-- JsonAPI helpers

displayIdInfo :: IdInfo -> Text
displayIdInfo (IdInfo IdProp {..} scope) =
    "'" <> idName <> "' " <> displayNameSpace idSpace <> " " <>
    case scope of
      Binder -> "binding"
      Local -> "defined locally" <>
        case idDefSpan of
          ProperSpan ss -> ", at " <> tshow ss
          _ -> ""
      Imported {..} ->
        "imported from " <> displayModuleId idImportedFrom <>
        (if idDefinedIn /= idImportedFrom
          then ", and defined in " <> displayModuleId idDefinedIn
          else "")
      WiredIn -> "builtin defined in " <> displayModuleId idDefinedIn

displayNameSpace :: IdNameSpace -> Text
displayNameSpace VarName = "value"
displayNameSpace DataName = "data constructor"
displayNameSpace TvName = "type variable"
displayNameSpace TcClsName = "type"

displayModuleId :: ModuleId -> Text
displayModuleId (ModuleId mo pkg) = mo <> " (" <> displayPackageId pkg <> ")"

displayPackageId :: PackageId -> Text
displayPackageId (PackageId name (Just version) _pkey) =
  name <> "-" <> cleanPackageVersion version
displayPackageId (PackageId name Nothing _pkey) =
  name

-- | In our environment, ghc-prim / base / etc has a name like this
--
-- ghc-prim-0.3.1.0-3f9f683cd77bf581e40e5d3162876874
--
-- It seems like a good hack for now to just strip the "-hash" off any
-- such package versions.  Consider moving it to ide-backend-client?
cleanPackageVersion :: Text -> Text
cleanPackageVersion x@(T.stripPrefix "-" . T.takeEnd 33 -> Just hash)
  | T.all isHexDigit hash = T.dropEnd 33 x
cleanPackageVersion x = x

$(makePrisms ''Response)
