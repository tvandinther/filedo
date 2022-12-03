module Commands.Compile (
    CompileOptions(..),
    compileInfo
) where

import Options.Applicative
import Types ( Directory(..) )
import Extensions ( directory )

data CompileOptions = CompileOptions
    { dataFiles :: [FilePath]
    , targetDirectory :: Directory
    , outputDirectory :: Directory
    , suppressWarnings :: Bool }
    deriving (Show)

compileInfo :: ParserInfo CompileOptions
compileInfo = info compileOptions (progDesc "Compile mustache templates")

compileOptions :: Parser CompileOptions
compileOptions =
    CompileOptions <$>
    dataFilesOption
    <*> targetDirectoryArgument
    <*> outputDirectoryOption
    <*> suppressWarningsOption

targetDirectoryArgument :: Parser Directory
targetDirectoryArgument = argument directory (metavar "TARGET_DIRECTORY" <> help "Directory containing mustache templates")

dataFilesOption :: Parser [FilePath]
dataFilesOption = many (strOption 
    ( long "data" 
    <> short 'd' 
    <> metavar "DATAFILES..." 
    <> help "Data files to use. Last argument takes merge precedence. Stdin will be used if none are specified." ))

outputDirectoryOption :: Parser Directory
outputDirectoryOption = option directory 
    ( long "output" 
    <> short 'o' 
    <> metavar "OUTPUTDIR" 
    <> value (Directory default')
    <> help ("Directory for compiled template output. Default: " ++ default') )
    where
        default' = "_build"

suppressWarningsOption :: Parser Bool
suppressWarningsOption = switch 
    ( short 'w'
    <> help "Suppress warnings." )
