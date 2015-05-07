module Types where

import           Control.Lens (Prism')
import qualified Control.Lens as Lens
import           Data.Text (Text)
import           IdeSession.Client.JsonAPI
import           IdeSession.Types.Progress
import           IdeSession.Types.Public
import           React.Ace (Ace)

data State = State
  { _stateAce :: Ace
  , _stateStatus :: Maybe Status
  , _stateRunning :: Running
  , _stateTab :: Tab
  , _stateInfo :: Text
  -- FIXME: this will be removed once a real terminal is used.
  , _stateConsole :: [Text]
  } deriving (Eq, Show)

data Status
  = BuildRequested [(FilePath, Text)]
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
  | InfoTab
  deriving (Eq, Show)

$(Lens.makeLenses ''State)
$(Lens.makePrisms ''Status)
