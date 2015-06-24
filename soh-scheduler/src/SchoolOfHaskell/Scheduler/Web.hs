{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module SchoolOfHaskell.Scheduler.Web where

import Airship
import Airship.Resource.Static (StaticOptions(..), staticResource)
import BasePrelude hiding (Handler, catch, mask, try)
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class (lift)
import Data.Aeson (encode, eitherDecode)
import Data.ByteString.Builder (lazyByteString)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import qualified Data.UUID as UUID
import Network.HTTP.Media (MediaType)
import qualified Network.HTTP.Types as HTTP
import Network.Wai.Handler.Warp
       (runSettings, defaultSettings, setPort, setHost)
import SchoolOfHaskell.Scheduler.API
import SchoolOfHaskell.Scheduler.Types
import SchoolOfHaskell.Scheduler.AWS
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors, simpleCorsResourcePolicy)
import qualified Network.Wai as Wai

data State =
  State {_sSettings :: Settings}
$(makeLenses ''State)

startDiscoverEnv :: String -> String -> IO ()
startDiscoverEnv region cluster =
  start (fromString cluster) =<<
  discoverEnv (fromString region)

startProfileEnv :: String -> String -> String -> IO ()
startProfileEnv profile region cluster =
  start (fromString cluster) =<<
  profileEnv (fromString profile) (fromString region)

startKeysEnv :: String -> String -> String -> String -> IO ()
startKeysEnv access secret region cluster =
  start (fromString cluster) =<<
  keysEnv (fromString access) (fromString secret) (fromString region)

startSessionEnv :: String -> String -> String -> String -> String -> IO ()
startSessionEnv access secret token region cluster =
  start (fromString cluster) =<<
  sessionEnv (fromString access)
             (fromString secret)
             (fromString token)
             (fromString region)

------------------------------------------------------------------------------

start :: Text -> Env -> IO ()
start ecs env' =
  do static <- staticResource staticOptions "static"
     let state =
           State (mkSettings ecs env')
     runSettings
       (setPort 3000 (setHost "0.0.0.0" defaultSettings))
       (cors corsPolicy
             (resourceToWai
                (do "static" </> star #> static
                    "containers" #> containerIndex
                    "containers" </>
                      var "id" #>
                      containerDetail)
                resource404
                state))

-- This allows pages served by other domains to access this API.
--
-- Note: It doesn't seem to be necessary, but ideally this would do
-- per-route computation of fields like 'corsMethods'
corsPolicy :: Wai.Request -> Maybe CorsResourcePolicy
corsPolicy _ = Just $ simpleCorsResourcePolicy
  { corsMethods = HTTP.methodDelete : corsMethods simpleCorsResourcePolicy
  , corsRequestHeaders = "Content-Type" : corsRequestHeaders simpleCorsResourcePolicy
  }

containerIndex :: forall m.
                  MonadIO m
               => Resource State m
containerIndex =
  resource {allowedMethods =
              return [HTTP.methodGet,HTTP.methodPost]
           ,contentTypesAccepted =
              return [(jsonMIME,return ())]
           ,contentTypesProvided =
              let cIndex =
                    do state <- getState
                       results <-
                         liftIO (runStdoutLoggingT
                                   (listContainers (state ^. sSettings)))
                       case results of
                         Left e ->
                           do putResponseBody (ResponseBuilder (fromString (show e)))
                              halt HTTP.status502
                         Right ids ->
                           return (ResponseBuilder (lazyByteString (encode ids)))
              in return [(jsonMIME,cIndex)]
           ,processPost =
              do state <- getState
                 req <- request
                 body <-
                   lift (entireRequestBody req)
                 case eitherDecode body of
                   Left e ->
                     do putResponseBody (ResponseBuilder (fromString (show e)))
                        halt HTTP.status400
                   Right spec' ->
                     do receipt <-
                          liftIO (runStdoutLoggingT
                                    (createContainer
                                       (state ^. sSettings)
                                       spec'))
                        case receipt of
                          Left e ->
                            do putResponseBody (ResponseBuilder (fromString (show e)))
                               halt HTTP.status502
                          Right receipt' ->
                            do putResponseBody (ResponseBuilder (lazyByteString (encode receipt')))
                               return (PostProcess (return ()))}

containerDetail :: forall m.
                   MonadIO m
                => Resource State m
containerDetail =
  resource {allowedMethods =
              return [HTTP.methodGet,HTTP.methodDelete]
           ,contentTypesProvided =
              do let cIndex =
                       do state <- getState
                          results <-
                            withReceiptOrId
                              (\id' ->
                                 liftIO (runStdoutLoggingT
                                           (getContainerDetail
                                              (state ^. sSettings)
                                              id')))
                              (\rcpt ->
                                 liftIO (runStdoutLoggingT
                                           (getContainerDetail
                                              (state ^. sSettings)
                                              rcpt)))
                          case results of
                            Left e ->
                              do putResponseBody (ResponseBuilder (fromString (show e)))
                                 halt HTTP.status502
                            Right detail ->
                              return (ResponseBuilder (lazyByteString (encode detail)))
                 return [(jsonMIME,cIndex)]
           ,deleteResource =
              do do state <- getState
                    results <-
                      withReceiptOrId
                        (\id' ->
                           liftIO (runStdoutLoggingT
                                     (stopContainer (state ^. sSettings)
                                                    id')))
                        (\rcpt ->
                           liftIO (runStdoutLoggingT
                                     (stopContainer (state ^. sSettings)
                                                    rcpt)))
                    case results of
                      Left e ->
                        do putResponseBody (ResponseBuilder (fromString (show e)))
                           return False
                      Right _ -> return True}

resource :: forall s m.
            MonadIO m
         => Resource s m
resource =
  defaultResource {knownContentType =
                     contentTypeMatches [jsonMIME]
                  ,lastModified = Just <$> liftIO getCurrentTime}

resource404 :: forall s m.
               Resource s m
resource404 =
  defaultResource {knownContentType =
                     contentTypeMatches [jsonMIME]
                  ,resourceExists = return False}

jsonMIME :: MediaType
jsonMIME = "application/json"

withReceiptOrId :: forall a (m :: * -> *).
                   Monad m
                => (ContainerReceipt -> Webmachine State m a)
                -> (ContainerId -> Webmachine State m a)
                -> Webmachine State m a
withReceiptOrId forReceipt forId =
  do p <- params
     let id' = p HM.! "id"
     case (UUID.fromString (T.unpack id')) of
       Nothing -> forId (ContainerId id')
       Just rcpt ->
         forReceipt (ContainerReceipt rcpt)

staticOptions :: StaticOptions
staticOptions =
#if NO_CACHE
  NoCache
#else
  Cache
#endif
