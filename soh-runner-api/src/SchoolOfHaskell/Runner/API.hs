{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module SchoolOfHaskell.Runner.API
  ( RunnerRequest (..)
  , RunnerResponse (..)
  , webServerPort
  , backendPort
  ) where

import Data.Aeson.TH
import Data.Text (Text)
import Stack.Ide.JsonAPI (Sequenced, Request, Response)
import Prelude hiding ((.), id)

data RunnerRequest =
    RunnerRequestAuth Text
  | RunnerRequestPortListening Int
  | RunnerRequestClient (Sequenced Request)
  deriving (Show)

data RunnerResponse =
    RunnerResponseAuthFailure
  | RunnerResponseAuthSuccess
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
backendPort :: Int
backendPort = 4000

-- | Port used by the user's web server.
webServerPort :: Int
webServerPort = 3000
