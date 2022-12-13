module Runners.Process (
    runProcess
) where

import Commands.Process (ProcessOptions(..))
import qualified Data.Yaml as YAML
import Types (Directory(..), DataFileType (JSON))
import Data.Yaml (prettyPrintParseException)
import Actions.Process (process, ProcessJob(..), ProcessSuccess(..), ProcessError(..))
import System.Directory.Extra (listDirectories, listFiles)
import Types.Command (prettyPrintCmd)
import Control.Monad.Extra (concatMapM)
import System.FilePath (addTrailingPathSeparator, takeFileName, (</>))
import Runners.Compile (createJob, runCompile', getData)
import Commands.Compile (CompileOptions(..))
import Actions.Compile (CompileSuccess(..), CompileJob (..), compile, CompileError)
import Types.LazyFile (readLazy, LazyFile (..))
import Types.FileScoped (FileScoped(..), findFile)
import Data.Text.Encoding (encodeUtf8)
import Types.Rule (Rule)
import Data.List.NonEmpty as NE ( NonEmpty((:|)), head )
import qualified Data.Aeson as JSON

runProcess :: ProcessOptions -> IO ()
runProcess (ProcessOptions d rf _ [] w) = do
    rootRule <- YAML.decodeFileEither rf
    case rootRule of
        Left err -> putStrLn $ prettyPrintParseException err
        Right rule -> do
            processRule rule d
runProcess (ProcessOptions d rf _ dfs w) = processWithData d rf dfs w

processWithData :: Directory -> FilePath -> [FilePath] -> Bool -> IO ()
processWithData d rf dfs w = do
    mergedData <- getData dfs
    case mergedData of
        Left err -> print err
        Right jv -> do
            compileResult <- compileRule jv
            case compileResult of
                Left err -> print err
                Right (CompileSuccess{renderedTemplates=rt}) -> do
                    let eRule = YAML.decodeEither' $ encodeUtf8 $ value $ NE.head rt
                    case eRule of
                        Left err -> print err
                        Right rule -> do
                            processRule rule d
    where
        compileRule jv = do
            lazyRf <- readLazy ("/" </> rf) rf
            pure . compile $ CompileJob jv $ lazyRf:|[]

processRule :: Rule -> Directory -> IO ()
processRule r d = do
    allTargets <- listAllRecursive $ unDirectory d
    let res = process $ ProcessJob "" r allTargets
    case res of
        Left err -> putStrLn $ getMessage err
        Right cs -> putStrLn $ unlines $ numberedLines $ prettyPrintCmd <$> cmds cs
    where
        numberedLines = Prelude.zipWith (\n s -> show n ++ "| " ++ s) [(1::Int)..]

listDirectoriesRecursive :: FilePath -> IO [FilePath]
listDirectoriesRecursive path = concatMapM listDirectoriesRecursive =<< listDirectories path

listAllRecursive :: FilePath -> IO [FilePath]
listAllRecursive path = do
    files <- listFiles path
    dirs <- listDirectories path
    rest <- concatMapM listAllRecursive dirs
    return $ files ++ (addTrailingPathSeparator <$> dirs) ++ rest
