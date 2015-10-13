{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module SchoolOfHaskell.Runner.API
  ( RunnerRequest (..)
  , RunnerResponse (..)
  , defaultBackendPort
  ) where

import Data.Aeson.TH
import Data.Text (Text)
import Stack.Ide.JsonAPI (Sequenced, Request, Response)
import Prelude hiding ((.), id)

data RunnerRequest =
    RunnerRequestAuth Text
  | RunnerRequestOpenPort
  | RunnerRequestPortListening Int
  | RunnerRequestClient (Sequenced Request)
  deriving (Show)

data RunnerResponse =
    RunnerResponseAuthFailure
  | RunnerResponseAuthSuccess
  | RunnerResponseOpenPort Int
  | RunnerResponsePortIsListening
  | RunnerResponseClient (Sequenced Response)
  deriving (Show)

$(fmap concat $ mapM (deriveJSON defaultOptions)
  [ ''RunnerRequest
  , ''RunnerResponse
  ])

------------------------------------------------------------------------------
-- Constants

-- | Default port used by soh-runner to listen for websocket connections.
defaultBackendPort :: Int
defaultBackendPort = 4000
