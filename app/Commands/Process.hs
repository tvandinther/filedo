module Commands.Process (
    ProcessOptions(..),
    processInfo
) where

import Options.Applicative
import Types ( Directory(..) )
import Extensions ( directory )
import Commands.Compile (dataFilesOption, suppressWarningsFlag)

data ProcessOptions = ProcessOptions
    { targetDirectory :: Directory
    , ruleFile :: FilePath
    , compileTemplates :: Bool
    , dataFiles :: [FilePath]
    , suppressWarnings :: Bool }

processInfo :: ParserInfo ProcessOptions
processInfo = info processOptions (progDesc "Process filedo rules")

processOptions :: Parser ProcessOptions
processOptions = 
    ProcessOptions 
    <$> targetDirectoryArgument
    <*> ruleFileOption
    <*> compileTemplatesFlag
    <*> dataFilesOption
    <*> suppressWarningsFlag

targetDirectoryArgument :: Parser Directory
targetDirectoryArgument = argument directory 
    ( metavar "TARGET_DIRECTORY" 
    <> value (Directory ".")
    <> help "Directory to process. Defaults to current directory." )

ruleFileOption :: Parser FilePath
ruleFileOption = strOption 
    ( long "rule" 
    <> short 'r' 
    <> metavar "RULE_FILE" 
    <> value "filedo.yaml"
    <> help "YAML file containing rules. Defaults to 'filedo.yaml'")

compileTemplatesFlag :: Parser Bool
compileTemplatesFlag = switch 
    ( long "compile"
    <> short 'c'
    <> help "Compile mustache templates before processing." )
