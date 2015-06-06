module Types where

import Ace (Editor, Range)
import Control.Concurrent.STM (TChan)
import Control.Lens (makeLenses, makePrisms, makeWrapped)
import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Data.Text (Text)
import IdeSession.Client.JsonAPI
import IdeSession.Types.Progress
import IdeSession.Types.Public
import React.Unmanaged (Unmanaged)
import TermJs (TermJs)
import React.IFrame
import Data.Vector (Vector)
import Data.Typeable (Typeable)

data State = State
  { _stateSnippets :: !(Vector Snippet)
  , _stateConsole :: !(Unmanaged TermJs)
  , _stateWeb :: !(Unmanaged IFrame)
  , _stateStatus :: !Status
  , _stateRunning :: !Running
  , _stateTab :: !Tab
  , _stateDocs :: !(Maybe IdInfo)
  , _stateBackend :: !(Maybe Backend)
  } deriving (Eq, Show, Typeable)

data Snippet = Snippet
  { _snippetEditor :: !(Unmanaged Editor)
  , _snippetPosMap :: !PosMap
  , _snippetTypeInfo :: !(Maybe [ResponseAnnExpType])
  } deriving (Eq, Show, Typeable)

data Status
  = NeverBuilt
  | BuildRequested !BuildRequest
  | Building !SnippetId !(Maybe Progress)
  | Built !SnippetId !BuildInfo
  | QueryRequested !SnippetId !BuildInfo !Query
  deriving (Eq, Show, Typeable)

data BuildInfo = BuildInfo
  { buildErrors :: ![AnnSourceError]
  , buildWarnings :: ![AnnSourceError]
  , buildServerDieds :: ![AnnSourceError]
  }
  deriving (Eq, Show, Typeable)

data Query
  = QueryInfo !SourceSpan
  deriving (Eq, Show, Typeable)

data Running
  = NotRunning
  | Running
  deriving (Eq, Show, Typeable)

data Tab
  = BuildTab
  | ConsoleTab
  | DocsTab
  | WebTab
  deriving (Eq, Show, Typeable)

data BuildRequest = BuildRequest !SnippetId [(FilePath, Text)]
  deriving (Eq, Show, Typeable)

data Backend = Backend
  { backendRequestChan :: TChan Request
  , backendResponseChan :: TChan Response
  , backendProcessHandler :: IORef (Either RunResult ByteString -> IO ())
  } deriving (Typeable)

instance Eq Backend where
  _ == _ = True

instance Show Backend where
  showsPrec _ _ = showString "Backend conn waiter"

-- Note: Newer changes are towards the front of the list.
newtype PosMap = PosMap { unPosMap :: [PosChange] }
  deriving (Eq, Show, Typeable)

data PosChange = PosChange
  { oldRange :: !Range
  , newRange :: !Range
  }
  deriving (Eq, Show, Typeable)

newtype SnippetId = SnippetId { unSnippetId :: Int }
  deriving (Eq, Show, Enum, Ord, Typeable)

$(makeLenses ''State)
$(makeLenses ''Snippet)
$(makePrisms ''Status)
$(makeWrapped ''PosMap)
