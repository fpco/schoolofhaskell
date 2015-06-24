{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module SchoolOfHaskell.Scheduler.AWS where

import BasePrelude hiding (getEnv, (&))
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Error
import Control.Monad.Logger
import Control.Monad.Trans.AWS hiding (Env)
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.List as Conduit
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.AWS.Data as AWS
import Network.AWS.EC2
import Network.AWS.ECS
import SchoolOfHaskell.Scheduler.API
import SchoolOfHaskell.Scheduler.Types

discoverEnv :: forall (m :: * -> *).
               (Applicative m,Functor m,MonadIO m)
            => Text -> m Env
discoverEnv region' =
  do parsedRegion <- hoistFromText region'
     Env <$>
       liftIO (getEnv parsedRegion Discover)

profileEnv :: forall (m :: * -> *).
              (Applicative m,Functor m,MonadIO m)
           => Text -> Text -> m Env
profileEnv profile region' =
  do parsedRegion <- hoistFromText region'
     Env <$>
       liftIO (getEnv parsedRegion (FromProfile profile))

keysEnv :: forall (m :: * -> *).
           (Applicative m,Functor m,MonadIO m)
        => Text -> Text -> Text -> m Env
keysEnv access sekret region' =
  do parsedRegion <- hoistFromText region'
     Env <$>
       liftIO (getEnv parsedRegion
                      (FromKeys (AccessKey (encodeUtf8 access))
                                (SecretKey (encodeUtf8 sekret))))

sessionEnv :: forall (m :: * -> *).
              (Applicative m,Functor m,MonadIO m)
           => Text -> Text -> Text -> Text -> m Env
sessionEnv access sekret token region' =
  do parsedRegion <- hoistFromText region'
     Env <$>
       liftIO (getEnv parsedRegion
                      (FromSession (AccessKey (encodeUtf8 access))
                                   (SecretKey (encodeUtf8 sekret))
                                   (SecurityToken (encodeUtf8 token))))

mkSettings :: Text -> Env -> Settings
mkSettings = Settings

setCluster :: Text -> Settings -> Settings
setCluster = set ssCluster

mkContainerSpec :: Text -> ContainerSpec
mkContainerSpec = ContainerSpec

setImageName :: Text -> ContainerSpec -> ContainerSpec
setImageName = set csImageName

createContainer :: forall (m :: * -> *).
                   (MonadBaseControl IO m,MonadCatch m,MonadIO m)
                => Settings
                -> ContainerSpec
                -> m (Either Err ContainerReceipt)
createContainer settings spec =
  do ident <- liftIO UUID.nextRandom
     results <-
       runAWST (settings ^. ssEnv ^. env)
               (send (runTask (spec ^. csImageName) &
                      (rtStartedBy ?~
                       fromString (UUID.toString ident)) &
                      (rtOverrides ?~
                       (taskOverride & toContainerOverrides .~
                        [containerOverride & coCommand .~
                         [T.pack (UUID.toString ident)]])) &
                      (rtCount ?~ 1)))
     return (either (Left . ContainerProviderErr)
                    (\r ->
                       case r ^. rtrFailures of
                         fs@(_:_) ->
                           Left (ContainerFailureErr fs)
                         [] ->
                           Right (ContainerReceipt ident))
                    results)

listContainers :: forall (m :: * -> *).
                  (MonadBaseControl IO m,MonadCatch m,MonadIO m)
               => Settings -> m (Either Err [ContainerId])
listContainers settings =
  -- ListTasks will only return max 100 hits but also doesn't
  -- have an `instance AWSPager` so we have to do our own here.
  -- http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_ListTasks.html
  let req =
        listTasks &
        (ltCluster ?~
         (settings ^. ssCluster))
  in pager req [] =<<
     runAWST (settings ^. ssEnv ^. env)
             (send req)
  where pager _ _ (Left err) =
          return (Left (ContainerProviderErr err))
        pager req accum (Right result)
          | isJust (result ^. ltrNextToken) =
            pager req
                  (accum ++
                   map ContainerId (result ^. ltrTaskArns)) =<<
            runAWST (settings ^. ssEnv ^. env)
                    (send (req &
                           (ltNextToken .~
                            (result ^. ltrNextToken))))
        pager _ accum (Right result) =
          return (Right (accum ++
                         map ContainerId (result ^. ltrTaskArns)))

class ContainerBy a where
  getContainerDetail :: forall (m :: * -> *).
                        (MonadBaseControl IO m,MonadCatch m,MonadIO m,MonadLogger m)
                     => Settings
                     -> a
                     -> m (Either Err ContainerDetail)
  stopContainer :: forall (m :: * -> *).
                   (MonadBaseControl IO m,MonadCatch m,MonadIO m,MonadLogger m)
                => Settings -> a -> m (Either Err ())

instance ContainerBy ContainerReceipt where
  getContainerDetail settings (ContainerReceipt ident) =
    do cid <-
         getContainerIdFromUUID settings ident
       case cid of
         Left e -> return (Left e)
         Right cid' ->
           getContainerDetail settings cid'
  stopContainer settings (ContainerReceipt ident) =
    do cid <-
         getContainerIdFromUUID settings ident
       case cid of
         Left e -> return (Left e)
         Right cid' ->
           stopContainer settings cid'

instance ContainerBy ContainerId where
  getContainerDetail settings (ContainerId ident) =
    do descrResult <-
         runAWST (settings ^. ssEnv ^. env)
                 (send (describeTasks &
                        (dtCluster ?~
                         (settings ^. ssCluster)) &
                        (dtTasks .~
                         [ident])))
       case descrResult of
         Left e ->
           return (Left (ContainerProviderErr e))
         Right tasks
           | (tasks ^. dtrTasks) ==
               [] ->
             return (Left (ContainerAbsentErr))
         Right tasks ->
           case tasks ^. dtrFailures of
             fs@(_:_) ->
               return (Left (ContainerFailureErr fs))
             [] ->
               case tasks ^. dtrTasks of
                 [] ->
                   return (Left (ContainerAbsentErr))
                 (t:ts) ->
                   do unless (length ts == 0)
                             ($logWarn ("More than 1 AWS ECS Task " <>
                                        "for SoH container " <> ident <> " " <>
                                        fromString (show (t : ts))))
                      addrPort <-
                        addrPortForTask settings t ident
                      return (either Left
                                     (\a ->
                                        Right (ContainerDetail ident
                                                               a
                                                               (t ^. tLastStatus)))
                                     addrPort)
  stopContainer settings (ContainerId ident) =
    do result <-
         runAWST (settings ^. ssEnv ^. env)
                 (send (stopTask ident &
                        (stCluster ?~
                         (settings ^. ssCluster))))
       case result of
         Left e ->
           return (Left (ContainerProviderErr e))
         Right _ -> return (Right ())

cleanupContainers :: forall (m :: * -> *).
                     (MonadBaseControl IO m,MonadCatch m,MonadIO m)
                  => Settings -> m ()
cleanupContainers se = error "Not Implemented!"

------------------------------------------------------------------------------

addrPortForTask :: forall (m :: * -> *).
                   (MonadBaseControl IO m,MonadCatch m,MonadIO m,MonadLogger m)
                => Settings
                -> Task
                -> Text
                -> m (Either Err (Maybe (Text,PortMappings)))
addrPortForTask settings task' ident =
  do results <-
       runAWST (settings ^. ssEnv ^. env)
               (do let instanceArns =
                         toListOf (tContainerInstanceArn . folded) task'
                   instanceIds <-
                     toListOf ((dcirContainerInstances . folded) .
                               (ciEc2InstanceId . folded)) <$>
                     send (describeContainerInstances &
                           (dciContainerInstances .~ instanceArns))
                   paginate (describeInstances &
                             (di1InstanceIds .~ instanceIds)) $$
                     Conduit.concatMap
                       (toListOf ((dirReservations . folded) .
                                  (rInstances . folded) .
                                  (i1PublicIpAddress . folded))) =$=
                     Conduit.consume)
     case results of
       Left e ->
         return (Left (ContainerProviderErr e))
       Right [] -> return (Right Nothing)
       Right (host:hosts) ->
         do unless (null hosts)
                   ($logWarn ("More than 1 AWS ECS EC2 Instance" <>
                              "for SoH container " <> ident <> " " <>
                              fromString (show (host : hosts))))
            let bs = toListOf ((tContainers . folded) .
                               (cNetworkBindings . folded))
                              task'
                pms = mapMaybe (\x -> (,) <$> x ^. nbHostPort
                                          <*> x ^. nbContainerPort)
                               bs
            return (Right (Just (host, PortMappings pms)))

hoistFromText :: forall (m :: * -> *) a.
                 (Applicative m,AWS.FromText a)
              => Text -> m a
hoistFromText txt =
  case AWS.fromText txt of
    Left e -> throw (ParseErr e)
    Right x -> pure x

getContainerIdFromUUID :: forall (m :: * -> *).
                          (MonadBaseControl IO m,MonadCatch m,MonadIO m,MonadLogger m)
                       => Settings -> UUID -> m (Either Err ContainerId)
getContainerIdFromUUID settings ident =
  do let txt = fromString (UUID.toString ident)
     arnsResult <-
       runAWST (settings ^. ssEnv ^. env)
               (view ltrTaskArns <$>
                send (listTasks &
                      (ltCluster ?~
                       (settings ^. ssCluster)) &
                      (ltStartedBy ?~ txt)))
     -- NOTE: we don't care about pagination here.  We only wanted 1
     -- Task for the UUID. "Pages" of results isn't our expectation &
     -- we would consider any result with >1 Task suspect.
     case arnsResult of
       Left err ->
         return (Left (ContainerProviderErr err))
       Right [] ->
         return (Left (ContainerAbsentErr))
       Right (arn:arns) ->
         do unless (null arns)
                   ($logWarn ("More than 1 AWS ECS Task " <>
                              "for SoH container " <> txt <> " " <>
                              fromString (show (arn : arns))))
            return (Right (ContainerId arn))
