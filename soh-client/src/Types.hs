module Types where

import Control.Concurrent.STM (TChan)
import Control.Lens (makeLenses, makePrisms, makeWrapped)
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import IdeSession.Types.Public
import JavaScript.Ace (Editor, Range, MarkerId)
import JavaScript.IFrame
import JavaScript.TermJs (TermJs)
import JavaScript.Unmanaged (Unmanaged)
import Prelude
import SchoolOfHaskell.Runner.API (RunnerRequest, RunnerResponse)
import SchoolOfHaskell.Scheduler.API (PortMappings)
import Stack.Ide.JsonAPI

-- | The application state.  Ideally, this would entirely consist of
-- pure data.
--
-- However, for simplicity and efficiency it also contains some
-- references to mutable javascript objects.  These fields, currently
-- those which involve 'Unmanaged' and 'Backend', are mutated on
-- initialization, and otherwise always point to the same object.
data State = State
  { _stateSnippets :: !(Vector Snippet)
    -- ^ State of the code editors.
  , _stateConsole :: !(Unmanaged TermJs)
    -- ^ TermJs console component, used for interacting with processes
    -- and GHCI.
  , _stateWeb :: !(Unmanaged IFrame)
    -- ^ IFrame component used for snippet web output.
  , _stateDocs :: !(Unmanaged IFrame)
    -- ^ IFrame component used for browsing documentation.
  , _stateStatus :: !Status
    -- ^ Status of the backend - whether it's building, idle, or
    -- performing a query.
  , _stateRunning :: !Running
    -- ^ Whether or not a process is running.
  , _stateTab :: !Tab
    -- ^ Which tab is currently focused.
  , _stateBackend :: !(Maybe Backend)
    -- ^ Connection to the backend, used for compiling code, running
    -- it, and querying information about it.
  } deriving (Eq, Show, Typeable)

-- | State of a SoH editor.
data Snippet = Snippet
  { _snippetEditor :: !(Unmanaged Editor)
    -- ^ Ace editor component for this snippet.
  , _snippetPosMap :: !PosMap
    -- ^ Tracks how edits affect source positions, since the last
    -- compile.
  , _snippetTypeInfo :: !(Maybe ([ResponseAnnExpType], Int, Int, MarkerId))
    -- ^ When this is set to a 'Just' value, type info is displayed
    -- inline in the snippet.
  } deriving (Eq, Show, Typeable)

data Status
  = InitialStatus
    -- ^ Initial status, before code has been compiled.
  | BuildRequested !BuildRequest
    -- ^ A build has been requested.  'Model.mainLoop' /
    -- 'Model.runQueries' waits for this status and then sends off the
    -- request to the backend.
  | Building !SnippetId !UpdateStatus
    -- ^ This indicates progress on the build, which occurs after the
    -- build request.
  | Built !SnippetId !BuildInfo
    -- ^ Once the build completes, we get the build info (errors and
    -- warnings).
  | QueryRequested !SnippetId !BuildInfo !Query
    -- ^ After the build has completed, 'Model.runQueries' waits for
    -- this status, and then sends the query to the backend.
  | KillRequested !SnippetId !BuildInfo
    -- ^ Status while we're waiting for the process to be killed.
  deriving (Eq, Show, Typeable)

-- | Errors and warnings which result from a compile.
data BuildInfo = BuildInfo
  { buildErrors :: ![SourceError]
  , buildWarnings :: ![SourceError]
  , buildServerDieds :: ![SourceError]
  }
  deriving (Eq, Show, Typeable)

-- | Query to be sent to the backend.
data Query
  = QueryInfo !SourceSpan
  deriving (Eq, Show, Typeable)

-- | Whether or not the backend is currently running a process.
data Running
  = NotRunning
  | Running
  deriving (Eq, Show, Typeable)

-- | Identifiers for the different tabs in the controls.
data Tab
  = BuildTab
  | ConsoleTab
  | DocsTab
  | WebTab
  deriving (Eq, Show, Typeable)

-- | A request to build a set of files.
data BuildRequest = BuildRequest !SnippetId [(FilePath, Text)]
  deriving (Eq, Show, Typeable)

-- | State of the connection with the backend.  See "Model.Protocol".
data Backend = Backend
  { backendHost :: Text
  , backendPortMappings :: PortMappings
  , backendRequestChan :: TChan RunnerRequest
  , backendResponseChan :: TChan RunnerResponse
  , backendProcessHandler :: IORef (ProcessOutput -> IO ())
  } deriving (Typeable)

instance Eq Backend where
  _ == _ = True

instance Show Backend where
  showsPrec _ _ = showString "Backend conn waiter"

data ProcessOutput
  = ProcessOutput String
  | ProcessDone RunResult
  | ProcessListening

-- | Stores a list of source code location changes.  See "View.PosMap".
--
-- Note: Newer changes are towards the front of the list.
newtype PosMap = PosMap { unPosMap :: [PosChange] }
  deriving (Eq, Show, Typeable)

-- | Stores the position change caused by a text replacement.
data PosChange = PosChange
  { oldRange :: !Range
    -- ^ Range of the deleted text, in the old state of the buffer.
  , newRange :: !Range
    -- ^ Range of the added text, in the new state of the buffer.
  }
  deriving (Eq, Show, Typeable)

-- | ID of a snippet - each snippet in the page is given an ID
-- number.
newtype SnippetId = SnippetId { unSnippetId :: Int }
  deriving (Eq, Show, Enum, Ord, Typeable)

$(makeLenses ''State)
$(makeLenses ''Snippet)
$(makePrisms ''Status)
$(makeWrapped ''PosMap)
