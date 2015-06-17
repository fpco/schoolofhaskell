{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module SchoolOfHaskell.RunnerAPI
  ( RunnerRequest (..)
  , RunnerResponse (..)
  , runnerMessagePrefix
  ) where

import Control.Category
import Data.Monoid
import Data.StackPrism
import Data.StackPrism.TH
import Data.Text (Text)
import Language.JsonGrammar
import Prelude hiding ((.), id)
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import IdeSession.Client.JsonAPI (Request, Response)

data RunnerRequest =
    RunnerRequestAuth Text
  | RunnerRequestPortListening Int
  | RunnerRequestClient Request
  deriving (Show)

data RunnerResponse =
    RunnerResponseAuthFailure
  | RunnerResponseAuthSuccess
  | RunnerResponsePortIsListening
  | RunnerResponseClient Response
  deriving (Show)

$(fmap concat $ mapM deriveStackPrisms
  [ ''RunnerRequest
  , ''RunnerResponse
  ])

instance Json RunnerRequest where
  grammar = label "RunnerRequest" $ mconcat
    [ object $
        property "runnerRequest" "auth"
      . fromPrism _RunnerRequestAuth
      . prop "receipt"
    , object $
        property "runnerRequest" "portListening"
      . fromPrism _RunnerRequestPortListening
      . prop "port"
    -- Fallback on handling ide-backend-client requests.  This way we
    -- don't change the format of those messages.
    , fromPrism _RunnerRequestClient . grammar
    ]

instance Json RunnerResponse where
  grammar = label "RunnerResponse" $ mconcat
    [ object $
        property "runnerResponse" "authFailure"
      . fromPrism _RunnerResponseAuthFailure
    , object $
        property "runnerResponse" "authSuccess"
      . fromPrism _RunnerResponseAuthSuccess
    , object $
        property "runnerResponse" "portIsListening"
      . fromPrism _RunnerResponsePortIsListening
    -- Fallback on handling ide-backend-client responses.  This way we
    -- don't change the format of those messages.
    , fromPrism _RunnerResponseClient . grammar
    ]

runnerMessagePrefix :: IsString a => a
runnerMessagePrefix = "soh-runner: "
