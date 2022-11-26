module Options (
    Opts(..),
    optsParser
) where

import Options.Applicative
import Commands
import Commands.MergeData
import Commands.Compile

data Opts = Opts
    { verbose :: Bool
    , optCommand :: Command
    }

programOptions :: Parser Opts
programOptions =
    Opts <$> verbosityParser <*>
    hsubparser (mergeDataCommand <> compileCommand)
    where
        mergeDataCommand = command "merge-data" $ MergeData <$> mergeDataInfo
        compileCommand = command "compile" $ Compile <$> compileInfo

optsParser :: ParserInfo Opts
optsParser =
    info
        (helper <*> versionParser <*> programOptions)
        (fullDesc <> progDesc "Run commands for targeted files." <>
            header
                "filedo - Run commands for targeted files.")

versionParser :: Parser (a -> a)
versionParser = infoOption "0.0" (long "version" <> help "Show version" <> hidden)

verbosityParser :: Parser Bool
verbosityParser = switch
    ( long "verbose"
    <> short 'v'
    <> help "Enable verbose mode"
    <> hidden )