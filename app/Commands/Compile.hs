module Commands.Compile (
    CompileOptions(..),
    compileInfo
) where
import Options.Applicative
import Data.List.NonEmpty ( NonEmpty((:|)) )
import qualified Data.List.NonEmpty as NE
import System.FilePath (isValid)
import Types ( Directory(..) )

data CompileOptions = CompileOptions
    { dataFiles :: [FilePath]
    , targetDirectory :: Directory
    , outputDirectory :: Directory }
    deriving (Show)

compileInfo :: ParserInfo CompileOptions
compileInfo = info compileOptions (progDesc "Compile mustache templates")

compileOptions :: Parser CompileOptions
compileOptions =
    CompileOptions <$>
    dataFilesOption
    <*> targetDirectoryArgument
    <*> outputDirectoryOption

targetDirectoryArgument :: Parser Directory
-- targetDirectoryArgument = undefined
targetDirectoryArgument = argument dir (metavar "TARGET_DIRECTORY" <> help "Directory containing mustache templates")

dir :: ReadM Directory
dir = str >>= \s -> if isValid s then return $ Directory s else readerError "Invalid directory."

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

outputDirectoryOption :: Parser Directory
outputDirectoryOption = option dir 
    ( long "output" 
    <> short 'o' 
    <> metavar "OUTPUTDIR" 
    <> value (Directory "_build")
    <> help "Directory for compiled template output. If omitted, YAML docs or JSON lines will be sent to Stdout." )

someNE :: Alternative f => f a -> f (NonEmpty a)
-- someNE = fmap NE.fromList . some -- unsafe (but still safe) implementation
someNE = liftA2 (:|) <*> many


