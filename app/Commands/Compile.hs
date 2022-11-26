module Commands.Compile (
    CompileOptions(..)
    , compileInfo
) where
import Options.Applicative

data CompileOptions = CompileOptions
    { templateFiles :: [FilePath]
    , dataFiles :: [FilePath]
    , outputDirectory :: FilePath }
    deriving (Show)

compileInfo :: ParserInfo CompileOptions
compileInfo = info compileOptions (progDesc "Compile mustache templates")

compileOptions :: Parser CompileOptions
compileOptions =
    CompileOptions <$>
    templateFilesArgument
    <*> dataFilesOption
    <*> outputDirectoryOption

templateFilesArgument :: Parser [FilePath]
templateFilesArgument = some (argument str 
    ( metavar "TEMPLATEFILES..." 
    <> help "Mustache templates." ))

dataFilesOption :: Parser [FilePath]
dataFilesOption = some (strOption 
    ( long "data" 
    <> short 'd' 
    <> metavar "DATAFILES..." 
    <> help "Data files to use. Last argument takes merge precedence." ))

outputDirectoryOption :: Parser FilePath
outputDirectoryOption = strOption 
    ( long "output" 
    <> short 'o' 
    <> metavar "OUTPUTDIR" 
    <> value "_build"
    <> help "Directory for compiled template output." )