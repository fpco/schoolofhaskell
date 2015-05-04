{-# LANGUAGE OverloadedStrings #-}

module Import
    ( module Control.Applicative
    , module Control.Concurrent.STM
    , module Control.Lens
    , module IdeSession.Client.JsonAPI
    , module IdeSession.Types.Progress
    , module IdeSession.Types.Public
    , module React
    , module React.Lucid
    , module Types
    , module Data.Monoid
    , module Data.Foldable
    , Text
    , tshow
      -- * Simplified types
    , React
    , App
    , Component
      -- * Utilities
    , addWhen
    , setTVarIO
    , modifyTVarIO
    , waitForTVarIO
    ) where

-- Re-exported modules
import           Control.Applicative ((<$>), (<*>))
import           Control.Concurrent.STM
import           Control.Lens
import           IdeSession.Client.JsonAPI hiding (iso)
import           IdeSession.Types.Progress
import           IdeSession.Types.Public
import           React hiding (App, Component)
import           React.Lucid
import           Types
import           Data.Monoid
import           Data.Foldable (forM_)

import           Data.Text (Text, pack, unpack)

import qualified React.Internal

type React a = ReactT State IO a

type App = React.Internal.App State IO

type Component a = React.Internal.Component State a IO

addWhen :: Bool -> Text -> Text -> Text
addWhen True x y = y <> " " <> x
addWhen False _ y = y

tshow :: Show a => a -> Text
tshow = pack . show

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
waitForTVarIO v p = atomically $ do
    x <- readTVar v
    case x ^? p of
        Just y -> return y
        Nothing -> retry

-- viewTVarIO :: Getting a s a -> TVar s -> IO a
viewTVarIO g v =
  atomically
    (fmap (view g)
          (readTVar v))
