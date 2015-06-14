{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module SchoolOfHaskell.Scheduler.API where

import           Control.Applicative ((<$>), (<*>))
import           Control.Lens (makeLenses)
import           Data.Aeson (ToJSON(..), FromJSON(..))
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Data (Data)
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID

newtype ContainerSpec =
  ContainerSpec {_csImageName :: Text}
  deriving (Eq, Show, Data, Typeable)

newtype ContainerReceipt =
  ContainerReceipt {_crID :: UUID}
  deriving (Eq, Data, Typeable)

instance Show ContainerReceipt where
  show = show . _crID

newtype ContainerId =
  ContainerId {_ciID :: Text}
  deriving (Eq, Show, Ord, Data, Typeable)

data ContainerDetail =
  ContainerDetail {_cdID :: Text
                  ,_cdAddress :: Maybe (Text, Int)
                  ,_cdStatus :: Maybe Text}
  deriving (Eq, Show, Data, Typeable)

instance ToJSON UUID where
  toJSON = toJSON . UUID.toString

instance FromJSON UUID where
  parseJSON val = do
    str <- parseJSON val
    case UUID.fromString str of
      Nothing -> fail "Failed to parse UUID from JSON"
      Just x -> return x

$(let opts n = defaultOptions { fieldLabelModifier = drop n } in
  concat <$> mapM (\(n, x) -> (++) <$> makeLenses x <*> deriveJSON (opts n) x)
  [ (3, ''ContainerSpec)
  , (3, ''ContainerReceipt)
  , (3, ''ContainerId)
  , (3, ''ContainerDetail)
  ])
