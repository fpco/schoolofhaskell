module Types where

import Control.Lens (makeLenses, makePrisms)
import Data.Text (Text)
import IdeSession.Client.JsonAPI
import IdeSession.Types.Progress
import IdeSession.Types.Public
import React.Ace (Ace)

data State = State
  { _stateAce :: Ace
  , _stateStatus :: Maybe Status
  , _stateRunning :: Running
  , _stateTab :: Tab
  , _stateDocs :: Maybe ResponseSpanInfo
  , _stateTypes :: Maybe [ResponseExpType]
  -- FIXME: this will be removed once a real terminal is used.
  , _stateConsole :: [Text]
  } deriving (Eq, Show)

data Status
  = BuildRequested Files
  | Building (Maybe Progress)
  | Built BuildInfo
  | QueryRequested BuildInfo Query
  deriving (Eq, Show)

data BuildInfo = BuildInfo
  { buildErrors :: [SourceError]
  , buildWarnings :: [SourceError]
  , buildServerDieds :: [SourceError]
  }
  deriving (Eq, Show)

data Query
  = QueryInfo SourceSpan
  deriving (Eq, Show)

data Running
  = NotRunning
  | Running
  deriving (Eq, Show)

data Tab
  = BuildTab
  | ConsoleTab
  | DocsTab
  deriving (Eq, Show)

type Files = [(FilePath, Text)]

$(makeLenses ''State)
$(makePrisms ''Status)
