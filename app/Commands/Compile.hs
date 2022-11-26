module Commands.Compile (
    CompileOptions(..)
    , compileInfo
) where
import Options.Applicative

data CompileOptions = CompileOptions
    { outputFile' :: Maybe FilePath }
    deriving (Show)

compileInfo :: ParserInfo CompileOptions
compileInfo = info compileOptions (progDesc "Compile mustache templates")

compileOptions :: Parser CompileOptions
compileOptions =
    CompileOptions <$>
    optional (strOption (long "output" <> short 'o' <> metavar "OUTPUT"))