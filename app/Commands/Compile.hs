module Commands.Compile (
    CompileOptions(..),
    compileInfo
) where
import Options.Applicative
import Data.List.NonEmpty ( NonEmpty((:|)) )
import qualified Data.List.NonEmpty as NE
import System.FilePath (isValid)

-- TODO: Allow templateFile :: FilePath, outputDirectory :: Maybe FilePath OR current (many files to required directory)
data CompileOptions = CompileOptions
    { dataFiles :: [FilePath]
    , targetDirectory :: FilePath
    -- , templatePartialFiles :: [FilePath]
    , outputDirectory :: FilePath }
    deriving (Show)

compileInfo :: ParserInfo CompileOptions
compileInfo = info compileOptions (progDesc "Compile mustache templates")

compileOptions :: Parser CompileOptions
compileOptions =
    CompileOptions <$>
    dataFilesOption
    <*> targetDirectoryArgument
    -- <*> templatePartialFilesOption
    <*> outputDirectoryOption

targetDirectoryArgument :: Parser FilePath
targetDirectoryArgument = argument dir (metavar "TARGET_DIRECTORY" <> help "Directory containing mustache templates")

dir :: ReadM FilePath
dir = str >>= \s -> if isValid s then return s else readerError "Invalid directory."

templateFilesArgument :: Parser (NonEmpty FilePath)
templateFilesArgument = someNE (argument str 
    ( metavar "TEMPLATEFILE" 
    <> help "Mustache template." ))

templatePartialFilesOption :: Parser [FilePath]
templatePartialFilesOption = many (strOption 
    ( long "partial"
    <> short 'p'
    <> metavar "TEMPLATEPARTIALFILE" 
    <> help "Mustache template partial." ))

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
    <> help "Directory for compiled template output. If omitted, YAML docs or JSON lines will be sent to Stdout." )

someNE :: Alternative f => f a -> f (NonEmpty a)
-- someNE = fmap NE.fromList . some -- unsafe (but still safe) implementation
someNE = liftA2 (:|) <*> many


