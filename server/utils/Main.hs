{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Main (main, main') where

import qualified Options.Applicative as OptParse
import qualified Page.Database as Database
import Page.Database.Utils (statistics)
import System.Log.FastLogger (newStderrLoggerSet, pushLogStrLn, toLogStr)

main :: IO ()
main = do
  configPath <- OptParse.execParser opts
  main' configPath
  where
    opts = OptParse.info (parser OptParse.<**> OptParse.helper) OptParse.fullDesc
    parser =
      OptParse.argument OptParse.str $
        OptParse.metavar "PATH"
          <> OptParse.help "Path to database file"

-- | Like 'main', but the path to the configuration
-- file is passed as an argument.
main' :: FilePath -> IO ()
main' fp = do
  loggerSet <- newStderrLoggerSet 0
  let infoLog = pushLogStrLn loggerSet . toLogStr
  db <- Database.load fp
  stats <- statistics db
  infoLog stats
