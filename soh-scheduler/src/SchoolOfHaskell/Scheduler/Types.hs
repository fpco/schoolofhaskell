{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module SchoolOfHaskell.Scheduler.Types where

import BasePrelude
import Control.Lens
import qualified Control.Monad.Trans.AWS as AWS
import Data.Text (Text)
import Network.AWS.ECS

newtype Env =
  Env {_env :: AWS.Env}

$(makeLenses ''Env)

data Settings =
  Settings {_ssCluster :: Text
           ,_ssEnv :: Env}

$(makeLenses ''Settings)

data Err
  = ContainerAbsentErr
  | ContainerProviderErr AWS.Error
  | ContainerHostMissingErr
  | ContainerPortMissingErr
  | ContainerFailureErr [Failure]
  | ParseErr String
  deriving (Show,Typeable)

instance Exception Err
