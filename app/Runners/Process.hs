module Runners.Process (
    runProcess
) where

import Commands.Process (ProcessOptions(..))
import qualified Data.Yaml as YAML
import Types (Directory(..))
import System.FilePath ((</>))
import Data.Yaml (prettyPrintParseException)
import Types.Rule (Rule)
import Actions.Process (process, ProcessJob(..), ProcessSuccess(..), ProcessError(..))
import System.Directory.Extra (listFilesRecursive, listDirectories, listFiles)
import Types.Command (prettyPrintCmd)
import qualified Debug.Trace as Debug
import Control.Monad.Extra (concatMapM)
import Data.Graph (path)
import Control.Monad (liftM2)

-- runProcess :: ProcessOptions -> IO ()
-- runProcess (ProcessOptions d rf c df w) = do
--     file <- YAML.decodeFileEither rf
--     case file of
--         Left err -> putStrLn $ prettyPrintParseException err
--         Right s -> print (s :: Rule)

runProcess :: ProcessOptions -> IO ()
runProcess (ProcessOptions d rf c df w) = do
    rootRule <- YAML.decodeFileEither rf
    allTargets <- listAllRecursive $ unDirectory d
    print allTargets
    case rootRule of
        Left err -> putStrLn $ prettyPrintParseException err
        Right s -> do
            -- print s
            let res = process $ ProcessJob "" s allTargets
            case res of
                Left err -> putStrLn $ getMessage err
                Right cs -> putStrLn $ unlines $ numberLines $ prettyPrintCmd <$> placeholder cs
    where
        numberLines :: [String] -> [String]
        numberLines = zipWith (\n s -> show n ++ "| " ++ s) [(1::Int)..]

listDirectoriesRecursive :: FilePath -> IO [FilePath]
listDirectoriesRecursive path = concatMapM listDirectoriesRecursive =<< (listDirectories path)

listAllRecursive :: FilePath -> IO [FilePath]
listAllRecursive path = do
    files <- listFiles path
    dirs <- listDirectories path
    print dirs
    rest <- concatMapM listAllRecursive dirs
    return $ files ++ dirs ++ rest