module Commands.Compile (
    CompileOptions(..),
    TemplateOptions(..),
    compileInfo
) where
import Options.Applicative

-- TODO: Allow templateFile :: FilePath, outputDirectory :: Maybe FilePath OR current (many files to required directory)
data CompileOptions = CompileOptions
    { dataFiles :: [FilePath]
    , template :: TemplateOptions }
    deriving (Show)

data TemplateOptions
    = SingleTemplate
        { templateFile :: FilePath
        , maybeOutputDirectory :: Maybe FilePath }
    | MultipleTemplates
        { templateFiles :: [FilePath]
        , outputDirectory :: FilePath }
    deriving (Show)

compileInfo :: ParserInfo CompileOptions
compileInfo = info compileOptions (progDesc "Compile mustache templates")

compileOptions :: Parser CompileOptions
compileOptions =
    CompileOptions <$>
    dataFilesOption
    <*> templateOptionsParser

templateOptionsParser :: Parser TemplateOptions
templateOptionsParser =
    (SingleTemplate <$> templateFileArgument <*> optional outputDirectoryOption)
    <|> (MultipleTemplates <$> templateFilesArgument <*> outputDirectoryOption)

templateFileArgument :: Parser FilePath
templateFileArgument = argument str (metavar "TEMPLATE" <> help "Mustache template file")

templateFilesArgument :: Parser [FilePath]
templateFilesArgument = some (argument str 
    ( metavar "TEMPLATEFILES..." 
    <> help "Mustache templates." ))

dataFilesOption :: Parser [FilePath]
dataFilesOption = many (strOption 
    ( long "data" 
    <> short 'd' 
    <> metavar "DATAFILES..." 
    <> help "Data files to use. Last argument takes merge precedence. Stdin will be used if none are specified." ))

outputDirectoryOption :: Parser FilePath
outputDirectoryOption = strOption 
    ( long "output" 
    <> short 'o' 
    <> metavar "OUTPUTDIR" 
    <> value "_build"
    <> help "Directory for compiled template output." )
