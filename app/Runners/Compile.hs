module Runners.Compile (
    runCompile
) where

import Commands.Compile (CompileOptions(..))
-- import qualified Data.ByteString.Char8 as BS
import Actions.MergeData
import Runners.MergeData(sendMergeJob, sendMergeJob')
import Types
import qualified Control.Arrow
import Actions.Compile (CompileError, compile, CompileSuccess(..), CompileJob(..))
import Data.Text
import qualified Data.Text.IO as TIO
import Actions.MergeData (mergeData')
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty)
import Types.LazyFile (LazyFile, readLazy)

runCompile :: CompileOptions -> IO ()
runCompile (CompileOptions dfs tfs o) = runCompile' dfs tfs o

runCompile' :: [FilePath] -> NonEmpty FilePath -> Maybe FilePath -> IO ()
runCompile' dfs tfs o = do
    df <- getData dfs
    case df of
        Left err -> print err
        Right d -> do
            lfs <- mapM readLazy tfs
            let result = compile $ CompileJob d lfs
            case result of
                Left err -> print err
                Right (CompileSuccess compiledbs) -> processOutput o compiledbs

compileTemplates :: NonEmpty LazyFile -> JSON.Value -> Either CompileError CompileSuccess
compileTemplates ts d = compile $ CompileJob d ts

-- change sendMergeJob interface to take bytestring instead of filepath
getData :: [FilePath] -> IO (Either String JSON.Value)
getData [] = BS.getContents >>= \bs -> return $ JSON.eitherDecodeStrict bs
getData dfs = do
    result <- sendMergeJob' JSON dfs
    return $ Control.Arrow.left errorMessage result

processOutput :: Maybe FilePath -> Text -> IO ()
processOutput Nothing txt = TIO.putStrLn txt
processOutput (Just path) txt = do
    TIO.writeFile path txt
    print $ "Output written to: " ++ show path
