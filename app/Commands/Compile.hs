module Commands.Compile
  ( CompileOptions (..),
    compileInfo,
    dataFilesOption,
    suppressWarningsFlag,
  )
where

import Extensions (directory)
import Options.Applicative
import Types (Directory (..))

data CompileOptions = CompileOptions
  { dataFiles :: [FilePath],
    targetDirectory :: Directory,
    outputDirectory :: Directory,
    suppressWarnings :: Bool
  }
  deriving (Show)

compileInfo :: ParserInfo CompileOptions
compileInfo = info compileOptions (progDesc "Compile mustache templates")

compileOptions :: Parser CompileOptions
compileOptions =
  CompileOptions
    <$> dataFilesOption
    <*> targetDirectoryArgument
    <*> outputDirectoryOption
    <*> suppressWarningsFlag

targetDirectoryArgument :: Parser Directory
targetDirectoryArgument = argument directory (metavar "TARGET_DIRECTORY" <> help "Directory containing mustache templates")

dataFilesOption :: Parser [FilePath]
dataFilesOption =
  many
    ( strOption
        ( long "data"
            <> short 'd'
            <> metavar "DATAFILES..."
            <> help "Data files to use. Last argument takes merge precedence. Stdin will be used if none are specified."
        )
    )

outputDirectoryOption :: Parser Directory
outputDirectoryOption =
  option
    directory
    ( long "output"
        <> short 'o'
        <> metavar "OUTPUTDIR"
        <> value (Directory default')
        <> help ("Directory for compiled template output. Default: " ++ default')
    )
  where
    default' = "_build"

suppressWarningsFlag :: Parser Bool
suppressWarningsFlag =
  switch
    ( short 'w'
        <> help "Suppress warnings."
    )
