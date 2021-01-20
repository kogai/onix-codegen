{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Lib
import Options.Applicative
import Util

data Opts = Opts
  { schemaPath :: String,
    schemaVersion :: String,
    language :: String
  }

myopts :: Parser Opts
myopts =
  Opts
    <$> strOption
      ( long "schemaPath"
          <> metavar "path/to/schema"
          <> help "Path to directory of schema"
      )
    <*> strOption
      ( long "schemaVersion"
          <> metavar "v2 or v3"
          <> help "Version of schema"
      )
    <*> strOption
      ( long "language"
          <> metavar "Go"
          <> help "Target language"
      )

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (myopts <**> helper)
        ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "onix-codegen - A code generaotor of ONIX for Books schema."
        )

run :: Opts -> IO ()
run Opts {schemaPath, schemaVersion = "v2", language = "go"} = render Go V2 schemaPath
run Opts {schemaPath, schemaVersion = "v3", language = "go"} = render Go V2 schemaPath
run Opts {schemaPath = _, schemaVersion = _, language = _} = throw Unimplemented
