module Types where

import Control.Concurrent.STM (TChan)
import Control.Lens (makeLenses, makePrisms)
import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Data.Text (Text)
import IdeSession.Client.JsonAPI
import IdeSession.Types.Progress
import IdeSession.Types.Public
import TermJs (TermJs)
import JavaScript.AceAjax.Raw.Types (Editor)
import React.Unmanaged (Unmanaged)

data State = State
  { _stateAce :: Unmanaged Editor
  , _stateStatus :: Maybe Status
  , _stateRunning :: Running
  , _stateTab :: Tab
  , _stateDocs :: Maybe IdInfo
  , _stateTypes :: Maybe [ResponseAnnExpType]
  , _stateConsole :: Unmanaged TermJs
  , _stateBackend :: Maybe Backend
  } deriving (Eq, Show)

data Status
  = BuildRequested Files
  | Building (Maybe Progress)
  | Built BuildInfo
  | QueryRequested BuildInfo Query
  deriving (Eq, Show)

data BuildInfo = BuildInfo
  { buildErrors :: [AnnSourceError]
  , buildWarnings :: [AnnSourceError]
  , buildServerDieds :: [AnnSourceError]
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

data Backend = Backend
  { backendRequestChan :: TChan Request
  , backendResponseChan :: TChan Response
  , backendProcessHandler :: IORef (Either RunResult ByteString -> IO ())
  }

instance Eq Backend where
  _ == _ = True

instance Show Backend where
  showsPrec _ _ = showString "Backend conn waiter"

$(makeLenses ''State)
$(makePrisms ''Status)
