module Runners.Process (
    runProcess
) where

import Commands.Process (ProcessOptions(..))
import qualified Data.Yaml as YAML
import Types (Directory(..))
import Data.Yaml (prettyPrintParseException)
import Actions.Process (process, ProcessJob(..), ProcessSuccess(..), ProcessError(..))
import System.Directory.Extra (listDirectories, listFiles)
import Types.Command (prettyPrintCmd)
import Control.Monad.Extra (concatMapM)
import System.FilePath (addTrailingPathSeparator)

runProcess :: ProcessOptions -> IO ()
runProcess (ProcessOptions d rf c df w) = do
    rootRule <- YAML.decodeFileEither rf
    allTargets <- listAllRecursive $ unDirectory d
    -- print allTargets
    case rootRule of
        Left err -> putStrLn $ prettyPrintParseException err
        Right s -> do
            let res = process $ ProcessJob "" s allTargets
            case res of
                Left err -> putStrLn $ getMessage err
                Right cs -> putStrLn $ unlines $ numberLines $ prettyPrintCmd <$> cmds cs
    where
        numberLines :: [String] -> [String]
        numberLines = zipWith (\n s -> show n ++ "| " ++ s) [(1::Int)..]

listDirectoriesRecursive :: FilePath -> IO [FilePath]
listDirectoriesRecursive path = concatMapM listDirectoriesRecursive =<< listDirectories path

listAllRecursive :: FilePath -> IO [FilePath]
listAllRecursive path = do
    files <- listFiles path
    dirs <- listDirectories path
    rest <- concatMapM listAllRecursive dirs
    return $ files ++ (addTrailingPathSeparator <$> dirs) ++ rest