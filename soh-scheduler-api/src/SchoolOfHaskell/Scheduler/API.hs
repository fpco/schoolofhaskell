{-# LANGUAGE TemplateHaskell #-}

module SchoolOfHaskell.Scheduler.API where

import           Control.Applicative ((<$>), (<*>))
import           Control.Lens (makeLenses)
import           Data.Aeson (ToJSON(..), FromJSON(..))
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Text (Text)
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID

newtype ContainerSpec =
  ContainerSpec {_csImageName :: Text}

newtype ContainerReceipt =
  ContainerReceipt {_crID :: UUID}

instance Show ContainerReceipt where
  show = show . _crID

newtype ContainerId =
  ContainerId {_ciID :: Text}

data ContainerDetail =
  ContainerDetail {_cdID :: Text
                  ,_cdAddress :: Maybe (Text, Int)
                  ,_cdStatus :: Maybe Text}

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
