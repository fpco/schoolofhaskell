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
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.List as Conduit
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.AWS.Data as AWS
import Network.AWS.EC2
import Network.AWS.ECS
import SchoolOfHaskell.Scheduler.Types

discoverEnv :: forall (m :: * -> *).
               (Applicative m,Functor m,MonadIO m)
            => Text -> m SchedulerEnv
discoverEnv region' =
  do parsedRegion <- hoistFromText region'
     SchedulerEnv <$>
       liftIO (getEnv parsedRegion Discover)

sessionEnv :: forall (m :: * -> *).
              (Applicative m,Functor m,MonadIO m)
           => Text -> Text -> Text -> Text -> m SchedulerEnv
sessionEnv access sekret token region' =
  do parsedRegion <- hoistFromText region'
     SchedulerEnv <$>
       liftIO (getEnv parsedRegion
                      (FromSession (AccessKey (encodeUtf8 access))
                                   (SecretKey (encodeUtf8 sekret))
                                   (SecurityToken (encodeUtf8 token))))

mkSchedulerSettings :: Text -> SchedulerEnv -> SchedulerSettings
mkSchedulerSettings = SchedulerSettings

setCluster :: Text -> SchedulerSettings -> SchedulerSettings
setCluster = set ssCluster

mkContainerSpec :: Text -> ContainerSpec
mkContainerSpec = ContainerSpec

setImageName :: Text -> ContainerSpec -> ContainerSpec
setImageName = set csImageName

createContainer :: forall (m :: * -> *).
                   (MonadBaseControl IO m,MonadCatch m,MonadIO m)
                => SchedulerSettings
                -> ContainerSpec
                -> m (Either SchedulerEx ContainerReceipt)
createContainer settings spec =
  do ident <- liftIO UUID.nextRandom
     results <-
       runAWST (settings ^. ssEnv ^. env)
               (send (runTask (spec ^. csImageName) &
                      (rtStartedBy ?~
                       fromString (UUID.toString ident)) &
                      (rtCount ?~ 1)))
     return (either (Left . ContainerProviderEx)
                    (\r ->
                       case r ^. rtrFailures of
                         fs@(_:_) ->
                           Left (ContainerFailureEx fs)
                         [] ->
                           Right (ContainerReceipt ident))
                    results)

listContainers :: forall (m :: * -> *).
                  (MonadBaseControl IO m,MonadCatch m,MonadIO m)
               => SchedulerSettings -> m (Either SchedulerEx [ContainerId])
listContainers settings =
  do arnsResult <-
       runAWST (settings ^. ssEnv ^. env)
               (view ltrTaskArns <$>
                send (listTasks &
                      (ltCluster ?~
                       (settings ^. ssCluster))))
     case arnsResult of
       Left err ->
         return (Left (ContainerProviderEx err))
       Right arns ->
         return (Right (map (ContainerId) arns))

class ContainerBy a where
  getContainerDetail :: forall (m :: * -> *).
                        (MonadBaseControl IO m,MonadCatch m,MonadIO m,MonadLogger m)
                     => SchedulerSettings
                     -> a
                     -> m (Either SchedulerEx ContainerDetail)
  stopContainer :: forall (m :: * -> *).
                   (MonadBaseControl IO m,MonadCatch m,MonadIO m,MonadLogger m)
                => SchedulerSettings -> a -> m (Either SchedulerEx ())

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
           return (Left (ContainerProviderEx e))
         Right tasks
           | (tasks ^. dtrTasks) ==
               [] ->
             return (Left (ContainerAbsentEx))
         Right tasks ->
           case tasks ^. dtrFailures of
             fs@(_:_) ->
               return (Left (ContainerFailureEx fs))
             [] ->
               case tasks ^. dtrTasks of
                 [] ->
                   return (Left (ContainerAbsentEx))
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
           return (Left (ContainerProviderEx e))
         Right _ -> return (Right ())

cleanupContainers :: forall (m :: * -> *).
                     (MonadBaseControl IO m,MonadCatch m,MonadIO m)
                  => SchedulerSettings -> m ()
cleanupContainers se = error "Not Implemented!"

------------------------------------------------------------------------------

addrPortForTask :: forall (m :: * -> *).
                   (MonadBaseControl IO m,MonadCatch m,MonadIO m,MonadLogger m)
                => SchedulerSettings
                -> Task
                -> Text
                -> m (Either SchedulerEx (Maybe (Text,Int)))
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
         return (Left (ContainerProviderEx e))
       Right [] -> return (Right Nothing)
       Right (host:hosts) ->
         do unless (null hosts)
                   ($logWarn ("More than 1 AWS ECS EC2 Instance" <>
                              "for SoH container " <> ident <> " " <>
                              fromString (show (host : hosts))))
            case (toListOf ((tContainers . folded) .
                            (cNetworkBindings . folded))
                           task') of
              [] -> return (Right Nothing)
              (binding:bindings) ->
                do unless (null bindings)
                          ($logWarn ("More than 1 AWS ECS Instance Network Binding " <>
                                     "for SoH container " <> ident <> " " <>
                                     fromString (show (binding : bindings))))
                   return (Right (fmap (host,)
                                       (binding ^. nbHostPort)))

hoistFromText :: forall (m :: * -> *) a.
                 (Applicative m,AWS.FromText a)
              => Text -> m a
hoistFromText txt =
  case AWS.fromText txt of
    Left e -> throw (ParseEx e)
    Right x -> pure x

getContainerIdFromUUID :: forall (m :: * -> *).
                          (MonadBaseControl IO m,MonadCatch m,MonadIO m,MonadLogger m)
                       => SchedulerSettings
                       -> UUID
                       -> m (Either SchedulerEx ContainerId)
getContainerIdFromUUID settings ident =
  do let txt = fromString (UUID.toString ident)
     arnsResult <-
       runAWST (settings ^. ssEnv ^. env)
               (view ltrTaskArns <$>
                send (listTasks &
                      (ltCluster ?~
                       (settings ^. ssCluster)) &
                      (ltStartedBy ?~ txt)))
     case arnsResult of
       Left err ->
         return (Left (ContainerProviderEx err))
       Right [] ->
         return (Left (ContainerAbsentEx))
       Right (arn:arns) ->
         do unless (null arns)
                   ($logWarn ("More than 1 AWS ECS Task " <>
                              "for SoH container " <> txt <> " " <>
                              fromString (show (arn : arns))))
            return (Right (ContainerId arn))
