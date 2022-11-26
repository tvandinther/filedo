module Main (main) where

import Options.Applicative
import Lib

main :: IO ()
main = someFunc

data Command
    = MergeData MergeDataOptions
    | Compile CompileOptions

-- data Options = Options
--     { mergeDataCommand :: Command
--     , compileCommand :: Command }



data MergeDataOptions = MergeDataOptions
    { dataFiles :: [FilePath]
    , outputFile :: Maybe FilePath }

data CompileOptions = CompileOptions
    { }

commands :: Parser Command
commands = hsubparser $ 
    command "merge-data" mergeDataInfo
    <> command "compile" compileInfo

mergeDataCommand :: Mod CommandFields MergeDataOptions
mergeDataCommand = command "merge-data" mergeDataInfo

mergeDataInfo :: ParserInfo MergeDataOptions
mergeDataInfo = info mergeDataParser (progDesc "Merges Data")

mergeDataParser :: Parser MergeDataOptions
mergeDataParser = MergeDataOptions
    <$> many (argument str (metavar "DATAFILES..."))
    <*> optional (strOption (long "output" <> short 'o' <> metavar "OUTPUT"))

compileInfo :: ParserInfo CompileOptions
compileInfo = undefined