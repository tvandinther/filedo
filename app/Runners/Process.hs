module Runners.Process (
    runProcess
) where

import Commands.Process (ProcessOptions(..))
import qualified Data.Yaml as YAML
import Types (Directory(..), DataFileType (JSON))
import Data.Yaml (prettyPrintParseException)
import Actions.Process (process, ProcessJob(..), ProcessSuccess(..), ProcessError(..))
import System.Directory.Extra (listDirectories, listFiles)
import Types.Command (prettyPrintCmd, printCmd, Cmd (..), Command (..))
import Control.Monad.Extra (concatMapM)
import System.FilePath (addTrailingPathSeparator, takeFileName, (</>))
import Runners.Compile (createJob, runCompile', getData, printWarnings)
import Commands.Compile (CompileOptions(..))
import Actions.Compile (CompileSuccess(..), CompileJob (..), compile, CompileError)
import Types.LazyFile (readLazy, LazyFile (..))
import Types.FileScoped (FileScoped(..), findFile)
import Data.Text.Encoding (encodeUtf8)
import Types.Rule (Rule)
import qualified Data.Set as Set
import System.Process.Extra (system)
import Options (GlobalOptions (quiet))
import qualified Control.Monad

runProcess :: GlobalOptions -> ProcessOptions -> IO ()
runProcess globalOpts (ProcessOptions d rf _ [] w dry) = do
    rootRule <- YAML.decodeFileEither rf
    case rootRule of
        Left err -> putStrLn $ prettyPrintParseException err
        Right rule -> do
            processRule rule d dry (quiet globalOpts)
runProcess globalOpts (ProcessOptions d rf _ dfs w dry) = processWithData d rf dfs w dry (quiet globalOpts)

processWithData :: Directory -> FilePath -> [FilePath] -> Bool -> DryRun -> Quiet -> IO ()
processWithData d rf dfs w dry q = do
    mergedData <- getData dfs
    case mergedData of
        Left err -> print err
        Right jv -> do
            lazyRf <- readLazy ("/" </> rf) rf
            let job = CompileJob jv $ Set.singleton lazyRf
            case compile job of
                Left err -> print err
                Right s -> do
                    eRule <- compiledRule lazyRf s
                    case eRule of
                        Left err -> print err
                        Right rule -> do
                            processRule rule d dry q
    where
        compiledRule lf (CompileSuccess get) = do
            let (warnings, template) = get lf
            Control.Monad.unless q $ printWarnings w warnings
            pure . YAML.decodeEither' . encodeUtf8 . value $ template

type DryRun = Bool
type Quiet = Bool

processRule :: Rule -> Directory -> DryRun -> Quiet -> IO ()
processRule r d True _ = do
    allTargets <- listAllRecursive $ unDirectory d
    let res = process $ ProcessJob "" r allTargets
    case res of
        Left err -> putStrLn $ getMessage err
        Right cs -> putStrLn $ unlines $ numberedLines $ prettyPrintCmd <$> cmds cs
    where
        numberedLines = Prelude.zipWith (\n s -> show n ++ "| " ++ s) [(1::Int)..]
processRule r d False q = do
    allTargets <- listAllRecursive $ unDirectory d
    let res = process $ ProcessJob "" r allTargets
    case res of
        Left err -> putStrLn $ getMessage err
        Right cs -> do
            mapM_ (runCmd q) $ cmds cs
    where
        runCmd True cmd = do
            runCommand cmd
        runCmd False cmd = do
            putStrLn $ "Running: " ++ printCmd cmd
            runCommand cmd

runCommand :: Cmd -> IO ()
runCommand (Unscoped (Command c)) = do
    _ <- system $ unwords c
    pure ()
runCommand (Scoped (FileScoped p (Command c))) = do
    _ <- system $ "export filepath=" ++ p ++ "; " ++ unwords c
    pure ()

listDirectoriesRecursive :: FilePath -> IO [FilePath]
listDirectoriesRecursive path = concatMapM listDirectoriesRecursive =<< listDirectories path

listAllRecursive :: FilePath -> IO [FilePath]
listAllRecursive path = do
    files <- listFiles path
    dirs <- listDirectories path
    rest <- concatMapM listAllRecursive dirs
    return $ files ++ (addTrailingPathSeparator <$> dirs) ++ rest
