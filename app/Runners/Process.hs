module Runners.Process
  ( runProcess,
  )
where

import Actions.Compile (CompileJob (..), CompileSuccess (..), compile)
import Actions.Process (ProcessError (..), ProcessJob (..), ProcessSuccess (..), process)
import Commands.Process (ProcessOptions (..))
import Control.Monad qualified
import Control.Monad.Extra (concatMapM)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (prettyPrintParseException)
import Data.Yaml qualified as YAML
import Options (GlobalOptions (quiet))
import Runners.Compile (getData, printWarnings)
import Shell (runCmds)
import System.Directory.Extra (listDirectories, listFiles)
import System.FilePath (addTrailingPathSeparator, (</>))
import System.Process.Extra (system)
import Types (Directory (..))
import Types.Command (Cmd (..), Command (..), prettyPrintCmd, printCmd)
import Types.FileScoped (FileScoped (..))
import Types.LazyFile (readLazy)
import Types.Rule (Rule (environment))

runProcess :: GlobalOptions -> ProcessOptions -> IO ()
runProcess globalOpts (ProcessOptions d rf _ [] _ dry) = do
  rootRule <- YAML.decodeFileEither rf
  case rootRule of
    Left err -> putStrLn $ prettyPrintParseException err
    Right r -> do
      processRule r d dry (quiet globalOpts)
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
            Right r -> do
              processRule r d dry q
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
    numberedLines = Prelude.zipWith (\n s -> show n ++ "| " ++ s) [(1 :: Int) ..]
processRule r d False q = do
  allTargets <- listAllRecursive $ unDirectory d
  let res = process $ ProcessJob "" r allTargets
  case res of
    Left err -> putStrLn $ getMessage err
    Right cs -> do
      runCmds (unDirectory d) (environment r) $ cmds cs

listAllRecursive :: FilePath -> IO [FilePath]
listAllRecursive path = do
  fs <- listFiles path
  ds <- listDirectories path
  rest <- concatMapM listAllRecursive ds
  return $ fs ++ (addTrailingPathSeparator <$> ds) ++ rest
