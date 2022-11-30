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
import qualified Data.List.NonEmpty as NE
import Types.LazyFile (LazyFile, readLazy)

runCompile :: CompileOptions -> IO ()
runCompile (CompileOptions dfs tfs pfs o) = runCompile' dfs tfs pfs o

runCompile' :: [FilePath] -> NonEmpty FilePath -> [FilePath] -> Maybe FilePath -> IO ()
runCompile' dfs tfs pfs o = do
    df <- getData dfs
    case df of
        Left err -> print err
        Right d -> do
            templates <- mapM readLazy tfs
            partials <- mapM readLazy pfs
            let result = compile $ CompileJob d templates partials
            case result of
                Left err -> print err
                Right (CompileSuccess rts ws) -> processOutput o rts

compileTemplates :: NonEmpty LazyFile -> JSON.Value -> Either CompileError CompileSuccess
compileTemplates ts d = compile $ CompileJob d ts []

-- change sendMergeJob interface to take bytestring instead of filepath
getData :: [FilePath] -> IO (Either String JSON.Value)
getData [] = BS.getContents >>= \bs -> return $ JSON.eitherDecodeStrict bs
getData dfs = do
    result <- sendMergeJob' JSON dfs
    return $ Control.Arrow.left errorMessage result

processOutput :: Maybe FilePath -> NonEmpty Text -> IO ()
-- processOutput = undefined
processOutput Nothing ts = TIO.putStrLn $ Data.Text.concat $ NE.toList (NE.intersperse separator ts)
    where
        separator = pack "\n---\n"
processOutput (Just fp) ts = TIO.writeFile fp $ Data.Text.concat $ NE.toList (NE.intersperse separator ts)
    where
        separator = pack "\n---\n"
-- processOutput (Just path) txt = do
--     TIO.writeFile path txt
--     print $ "Output written to: " ++ show path

-- TODO: keep file data alongside the template to reconstruct it in the output directory
-- TODO: Print warnings before output. Add flag to suppress warnings.
-- TODO: Add flag to allow specification of partials. If value is a directory, use all files in the directory recursively as partials.