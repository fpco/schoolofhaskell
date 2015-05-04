module Types where

import qualified Control.Lens as Lens
import           Data.Text (Text)
import           IdeSession.Client.JsonAPI
import           IdeSession.Types.Progress
import           IdeSession.Types.Public
import           JavaScript.WebSockets (Connection)
import           React.Ace (Ace)

newtype BackendConnection =
  BackendConnection { unBackendConnection :: Connection }

data State = State
  { _stateAce :: Ace
  , _stateStatus :: Maybe BuildStatus
  , _stateRunning :: Running
  , _stateTab :: Tab
  -- FIXME: this will be removed once a real terminal is used.
  , _stateConsole :: [Text]
  } deriving (Eq, Show)

data BuildStatus
  = BuildRequested [(FilePath, Text)]
  | Building (Maybe Progress)
  | Built BuildInfo
  deriving (Eq, Show)

data BuildInfo = BuildInfo
  { buildErrors :: [SourceError]
  , buildWarnings :: [SourceError]
  , buildServerDieds :: [SourceError]
  }
  deriving (Eq, Show)

data Running
  = NotRunning
  | Running
  deriving (Eq, Show)

data Tab
  = BuildTab
  | ConsoleTab
  | DocsTab
  -- | InfoTab
  deriving (Eq, Show)

$(Lens.makeLenses ''State)
$(Lens.makePrisms ''BuildStatus)
