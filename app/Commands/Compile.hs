module Commands.Compile (
    CompileOptions(..),
    compileInfo
) where
import Options.Applicative
import Data.List.NonEmpty ( NonEmpty((:|)) )
import qualified Data.List.NonEmpty as NE

-- TODO: Allow templateFile :: FilePath, outputDirectory :: Maybe FilePath OR current (many files to required directory)
data CompileOptions = CompileOptions
    { dataFiles :: [FilePath]
    , templateFiles :: NonEmpty FilePath
    , outputDirectory :: Maybe FilePath }
    deriving (Show)

compileInfo :: ParserInfo CompileOptions
compileInfo = info compileOptions (progDesc "Compile mustache templates")

compileOptions :: Parser CompileOptions
compileOptions =
    CompileOptions <$>
    dataFilesOption
    <*> templateFilesArgument
    <*> outputDirectoryOption

templateFilesArgument :: Parser (NonEmpty FilePath)
templateFilesArgument = someNE (argument str 
    ( metavar "TEMPLATEFILE" 
    <> help "Mustache template." ))

dataFilesOption :: Parser [FilePath]
dataFilesOption = many (strOption 
    ( long "data" 
    <> short 'd' 
    <> metavar "DATAFILES..." 
    <> help "Data files to use. Last argument takes merge precedence. Stdin will be used if none are specified." ))

outputDirectoryOption :: Parser (Maybe FilePath)
outputDirectoryOption = optional $ strOption 
    ( long "output" 
    <> short 'o' 
    <> metavar "OUTPUTDIR" 
    -- <> value "_build"
    <> help "Directory for compiled template output. If omitted, YAML docs or JSON lines will be sent to Stdout." )

someNE :: Alternative f => f a -> f (NonEmpty a)
-- someNE = fmap NE.fromList . some -- unsafe (but still safe) implementation
someNE = liftA2 (:|) <*> many
