module Commands.MergeData (
    MergeDataOptions(..)
    , mergeDataInfo
) where

import Options.Applicative
import Types ( DataFileType(..) )
import Data.Char (toLower)

data MergeDataOptions = MergeDataOptions
    { dataFiles :: [FilePath]
    , outputFile :: Maybe FilePath
    , outputType :: DataFileType }
    deriving (Show)

mergeDataInfo :: ParserInfo MergeDataOptions
mergeDataInfo = info mergeDataOptions (progDesc "Merge data files")

mergeDataOptions :: Parser MergeDataOptions
mergeDataOptions =
    MergeDataOptions <$>
    dataFilesArgument
    <*> outputFileOption
    <*> outputTypeOption

dataFilesArgument :: Parser [FilePath]
dataFilesArgument = some (argument str 
    ( metavar "DATAFILES..." 
    <> help "Data files to merge. Last argument takes merge precedence." ))

outputFileOption :: Parser (Maybe FilePath)
outputFileOption = optional (strOption 
    ( long "output" 
    <> short 'o' 
    <> metavar "OUTPUT"
    <> help "Output file for merged data." ))

outputTypeOption :: Parser DataFileType
outputTypeOption = option readOutputType
    ( long "type"
    <> short 't'
    <> metavar "TYPE"
    <> value JSON
    <> help "Output type for merged data. (json, yaml)" )

readOutputType :: ReadM DataFileType
readOutputType = str >>= \s -> case map toLower s of
    "json" -> return JSON
    "yaml" -> return YAML
    _ -> readerError $ "Cannot parse value '" ++ s ++ "'. Expecting one of: json, yaml"
