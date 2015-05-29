{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import BasePrelude hiding (Handler, catch, mask, try)
import Data.Time (getCurrentTime)
import Distribution.PackageDescription.TH
       (PackageDescription(package), PackageIdentifier(pkgVersion),
        packageVariable)
import Language.Haskell.TH (runIO, stringE)
import Options.Applicative
import SchoolOfHaskell.Scheduler

-- | Main entry point.
main :: IO ()
main = (void . join . execParser) optParser
  where optParser =
          info (helper <*>
                subparser (discoverCmd <> sessionCmd))
               (fullDesc <>
                header ("soh-scheduler " <> packageVersion <> " " <> buildDate) <>
                progDesc "School of Haskell Container Scheduler")
        discoverCmd =
          command "discover"
                  (info (helper <*>
                         (startDiscoverEnv <$> regionOpt <*> clusterOpt))
                        (fullDesc <>
                         progDesc "Discover AWS Credentials"))
        sessionCmd =
          command "session"
                  (info (helper <*>
                         (startSessionEnv <$> accessKeyOpt <*> secretKeyOpt <*>
                          sessionTokenOpt <*> regionOpt <*> clusterOpt))
                        (fullDesc <>
                         progDesc "Use an Existing AWS Session"))
        accessKeyOpt =
          strOption (long "access-key" <>
                     short 'a' <>
                     metavar "ACCESS" <>
                     help "AWS Access Key ID")
        secretKeyOpt =
          strOption (long "secret-key" <>
                     short 's' <>
                     metavar "SECRET" <>
                     help "AWS Secret Access Key")
        sessionTokenOpt =
          strOption (long "session-token" <>
                     short 't' <>
                     metavar "TOKEN" <>
                     help "AWS Session Token")
        regionOpt =
          strOption (long "region" <>
                     short 'r' <>
                     metavar "REGION" <>
                     help "AWS Region")
        clusterOpt =
          strOption (long "cluster" <>
                     short 'c' <>
                     metavar "CLUSTER" <>
                     help "AWS ECS Arn")

-- | Embed the project package version number in the code.
packageVersion :: String
packageVersion =
  $(packageVariable (pkgVersion . package))

-- | Embed the build date in the code.
buildDate :: String
buildDate =
  $(stringE =<<
    runIO (show `fmap` Data.Time.getCurrentTime))
