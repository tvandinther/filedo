module Commands.Process (
    ProcessOptions(..),
    processInfo
) where

import Options.Applicative
import Types ( Directory(..) )
import Extensions ( directory )

data ProcessOptions = ProcessOptions
    { targetDirectory :: Directory }

processInfo :: ParserInfo ProcessOptions
processInfo = info processOptions (progDesc "Process filedo rules")

processOptions :: Parser ProcessOptions
processOptions = 
    ProcessOptions <$> 
    targetDirectoryArgument

targetDirectoryArgument :: Parser Directory
targetDirectoryArgument = argument directory (metavar "TARGET_DIRECTORY" <> help "Directory to process")
