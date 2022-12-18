module Options
  ( GlobalOptions (..),
    optsParser,
  )
where

import Commands
import Commands.Compile
import Commands.MergeData
import Commands.Process
import Options.Applicative

data GlobalOptions = GlobalOptions
  { verbose :: Bool,
    quiet :: Bool,
    optCommand :: Command
  }

programOptions :: Parser GlobalOptions
programOptions =
  GlobalOptions
    <$> verbosityParser
    <*> quietParser
    <*> hsubparser (mergeDataCommand <> compileCommand <> processCommand)
  where
    mergeDataCommand = command "merge-data" $ MergeData <$> mergeDataInfo
    compileCommand = command "compile" $ Compile <$> compileInfo
    processCommand = command "process" $ Process <$> processInfo

optsParser :: ParserInfo GlobalOptions
optsParser =
  info
    (helper <*> versionParser <*> programOptions)
    ( fullDesc
        <> progDesc "Run commands for targeted files."
        <> header
          "filedo - Run commands for targeted files."
    )

versionParser :: Parser (a -> a)
versionParser = infoOption "0.0" (long "version" <> help "Show version" <> hidden)

quietParser :: Parser Bool
quietParser = switch (long "quiet" <> short 'q' <> help "Enable quiet mode")

verbosityParser :: Parser Bool
verbosityParser =
  switch
    ( long "verbose"
        <> short 'v'
        <> help "Enable verbose mode"
        <> hidden
    )
