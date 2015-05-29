{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module SchoolOfHaskell.Scheduler.Types where

import BasePrelude hiding (getEnv, (&))
import Control.Lens
import qualified Control.Monad.Trans.AWS as AWS
import Data.Aeson
import Data.Aeson.TH
       (deriveFromJSON, deriveToJSON, defaultOptions, fieldLabelModifier)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Network.AWS.ECS

newtype Env =
  Env {_env :: AWS.Env}

$(makeLenses ''Env)

data Settings =
  Settings {_ssCluster :: Text
           ,_ssEnv :: Env}

$(makeLenses ''Settings)

newtype ContainerSpec =
  ContainerSpec {_csImageName :: Text}

$(makeLenses ''ContainerSpec)

$(deriveFromJSON
    defaultOptions {fieldLabelModifier = drop 3}
    ''ContainerSpec)

instance ToJSON UUID where
  toJSON = toJSON . UUID.toString

newtype ContainerReceipt =
  ContainerReceipt {_crID :: UUID}

$(makeLenses ''ContainerReceipt)

$(deriveToJSON
    defaultOptions {fieldLabelModifier = drop 3}
    ''ContainerReceipt)

instance Show ContainerReceipt where
  show = show . _crID

newtype ContainerId =
  ContainerId {_ciID :: Text}

$(makeLenses ''ContainerId)

$(deriveToJSON
    defaultOptions {fieldLabelModifier = drop 3}
    ''ContainerId)

data ContainerDetail =
  ContainerDetail {_cdID :: Text
                  ,_cdAddress :: Maybe (Text, Int)
                  ,_cdStatus :: Maybe Text}

$(makeLenses ''ContainerDetail)

$(deriveToJSON
    defaultOptions {fieldLabelModifier = drop 3}
    ''ContainerDetail)

data Err
  = ContainerAbsentErr
  | ContainerProviderErr AWS.Error
  | ContainerHostMissingErr
  | ContainerPortMissingErr
  | ContainerFailureErr [Failure]
  | ParseErr String
  deriving (Show,Typeable)

instance Exception Err
