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
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Actions.MergeData (mergeData')
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Types.LazyFile (LazyFile, readLazy)
import System.Directory.Extra (listFilesRecursive)
import Data.List (intersperse)
import qualified Data.List as L
import System.FilePath ((</>))
import System.Directory.Extra (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)
import Types (Directory)

type TargetDirectory = Directory
type OutputDirectory = Directory

runCompile :: CompileOptions -> IO ()
runCompile (CompileOptions dfs td od) = runCompile' dfs td od

runCompile' :: [FilePath] -> TargetDirectory -> OutputDirectory -> IO ()
runCompile' dfs td od = do
    df <- getData dfs
    case df of
        Left err -> print err
        Right d -> do
            files <- listFilesRecursive $ unDirectory td
            templates <- mapM (readLazy $ unDirectory td) files
            let result = compile $ CompileJob d templates
            case result of
                Left err -> print err
                Right (CompileSuccess rts ws) -> processOutput od ws rts

getData :: [FilePath] -> IO (Either String JSON.Value)
getData [] = BS.getContents >>= \bs -> return $ JSON.eitherDecodeStrict bs
getData dfs = do
    result <- sendMergeJob' JSON dfs
    return $ Control.Arrow.left errorMessage result

processOutput :: OutputDirectory -> [(FilePath, String)] -> [(FilePath, Text)] -> IO ()
-- processOutput Nothing ws ts = do
--     putStrLn $ Prelude.unlines ws
--     TIO.putStrLn $ Data.Text.concat $ L.intersperse (pack "\n---\n") $ snd <$> ts
processOutput od ws ts = do
    putStrLn $ "There are " ++ show (Prelude.length ws) ++ " warnings:"
    putStrLn $ Prelude.unlines $ catWarning <$> ws
    mapM_ processFile ts
    where
        root = unDirectory od
        catWarning (wp, w) = wp ++ ": " ++ w
        processFile (p, t)
            | T.length t > 0 = do
            let path = root </> p
            createDirectoryIfMissing True $ takeDirectory path
            putStrLn $ "Writing " ++ path
            TIO.writeFile path t
            | otherwise = putStrLn $ "Skipping " ++ p ++ " because it is empty"

-- TODO: Add flag to suppress warnings.