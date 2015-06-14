{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad (void)
import SchoolOfHaskell.Runner (runner, Settings(Settings))
import Options.Applicative.Simple
import Paths_soh_runner (version)
import Data.Text (pack)

main :: IO ()
main = id . snd =<<
  simpleOptions
    $(simpleVersion version)
    "soh-runner"
    "School of Haskell ide-backend-client runner"
    (pure ())
    (addCommand
      "run"
      "Run the soh-runner server"
      (\settings -> runner settings)
      settingsParser)

settingsParser :: Parser Settings
settingsParser =
  Settings
    <$> option auto (long "port" <> short 'p' <> metavar "PORT" <> value 4000)
    <*> (pack <$> option str (long "receipt" <> short 'r' <> metavar "RECEIPT"))
