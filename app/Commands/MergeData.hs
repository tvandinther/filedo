module Commands.MergeData (
    MergeDataOptions(..)
    , mergeDataInfo
) where

import Options.Applicative

data MergeDataOptions = MergeDataOptions
    { dataFiles :: [FilePath]
    , outputFile :: Maybe FilePath }
    deriving (Show)

mergeDataInfo :: ParserInfo MergeDataOptions
mergeDataInfo = info mergeDataOptions (progDesc "Merge data files")

mergeDataOptions :: Parser MergeDataOptions
mergeDataOptions =
    MergeDataOptions <$>
    many (argument str (metavar "DATAFILES..."))
    <*> optional (strOption (long "output" <> short 'o' <> metavar "OUTPUT"))