{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import SchoolOfHaskell.Runner (runner, Settings(Settings))
import Options.Applicative.Simple
import Paths_soh_runner (version)
import Data.Text (pack)
import SchoolOfHaskell.Runner.API (backendPort)

main :: IO ()
main = id . snd =<<
  simpleOptions
    $(simpleVersion version)
    "soh-runner"
    "School of Haskell stack runner"
    (pure ())
    (addCommand
      "run"
      "Run the soh-runner server"
      runner
      settingsParser)

settingsParser :: Parser Settings
settingsParser =
  Settings
    <$> option auto (long "port" <> short 'p' <> metavar "PORT" <> value backendPort)
    <*> option (fmap pack str) (long "receipt" <> short 'r' <> metavar "RECEIPT")
    <*> optional (option auto (long "lifetime-seconds" <> metavar "SECONDS"))
    <*> switch (long "verbose" <> short 'v')
