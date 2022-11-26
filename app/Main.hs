-- {-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Lib
import Data.Semigroup ((<>))
import Options.Applicative

data Opts = Opts
    { verbose :: Bool
    , optCommand :: Command
    }

data MergeDataOptions = MergeDataOptions
    { dataFiles :: [FilePath]
    , outputFile :: Maybe FilePath }
    deriving (Show)

data CompileOptions = CompileOptions
    { outputFile' :: Maybe FilePath }
    deriving (Show)

data Command
    = MergeData MergeDataOptions
    | Compile CompileOptions

main :: IO ()
main = do
    opts <- execParser optsParser
    case optCommand opts of
        MergeData x -> print x
        Compile x -> print x
    putStrLn ("global flag: " ++ show (verbose opts))

optsParser :: ParserInfo Opts
optsParser =
    info
        (helper <*> versionParser <*> programOptions)
        (fullDesc <> progDesc "optparse subcommands example" <>
            header
                "optparse-sub-example - a small example program for optparse-applicative with subcommands")

versionParser :: Parser (a -> a)
versionParser = infoOption "0.0" (long "version" <> help "Show version")

verbosityParser :: Parser Bool
verbosityParser = switch
    ( long "verbose"
    <> short 'v'
    <> help "Enable verbose mode" )

programOptions :: Parser Opts
programOptions =
    Opts <$> verbosityParser <*>
    hsubparser (mergeDataCommand <> compileCommand)

mergeDataCommand :: Mod CommandFields Command
mergeDataCommand =
    command
        "merge-data"
        (info (MergeData <$> mergeDataOptions) (progDesc "Create a thing"))

-- mergeDataOptions :: Parser MergeDataOptions
mergeDataOptions :: Parser MergeDataOptions
mergeDataOptions =
    MergeDataOptions <$>
    many (argument str (metavar "DATAFILES..."))
    <*> optional (strOption (long "output" <> short 'o' <> metavar "OUTPUT"))

compileCommand :: Mod CommandFields Command
compileCommand =
    command
        "compile"
        (info (Compile <$> compileOptions) (progDesc "Delete the thing"))

compileOptions :: Parser CompileOptions
compileOptions =
    CompileOptions <$>
    optional (strOption (long "output" <> short 'o' <> metavar "OUTPUT"))