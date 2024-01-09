module Runners.Process
  ( runProcess,
  )
where

import Actions.Compile
  ( CompileError,
    CompileJob (..),
    CompileSuccess (..),
    compile,
  )
import Actions.Process
  ( ProcessError (..),
    ProcessJob (..),
    ProcessSuccess (..),
    process,
  )
import Commands.Process (ProcessOptions (..))
import Commands.Process qualified
import Commands.Compile (CompileOptions (..))
import Commands.Compile qualified
import Control.Monad qualified
import Control.Monad.Extra (concatMapM)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (prettyPrintParseException)
import Data.Yaml qualified as YAML
import GHC.Base (lazy)
import Options (GlobalOptions (quiet))
import Runners.Compile (getData, printWarnings, runCompile)
import Shell (runCmds)
import System.Directory.Extra (listDirectories, listFiles)
import System.FilePath (addTrailingPathSeparator, (</>))
import System.Process.Extra (system)
import Types (Directory (..))
import Types.Command
  ( Command (..),
    prettyPrintQualifiedCommand,
    printQualifiedCommand,
  )
import Types.FileScoped (FileScoped (..))
import Types.LazyFile (readLazy, mkLazyFile)
import Types.Rule (Rule (environment))

runProcess :: GlobalOptions -> ProcessOptions -> IO ()
runProcess globalOpts (ProcessOptions d rf c [] w dry) = do
  _ <- runCompile CompileOptions [] d d w
  rootRule <- YAML.decodeFileEither rf
  case rootRule of
    Left err -> putStrLn $ prettyPrintParseException err
    Right r -> do
      processRule r d dry (quiet globalOpts)
runProcess globalOpts (ProcessOptions d rf c dfs w dry) = processWithData d rf dfs w dry (quiet globalOpts)

handleCompileFlag :: ProcessOptions -> IO ()
handleCompileFlag (ProcessOptions d _ True dfs w _) =
  runCompile CompileOptions dfs d d w
handleCompileFlag (ProcessOptions _ _ False _ _ _) = pure ()

-- processWithData :: Directory -> FilePath -> [FilePath] -> Bool -> DryRun -> Quiet -> IO ()
processWithData :: GlobalOptions -> ProcessOptions -> IO ()
processWithData gopts opts = do
  mergedData <- getData (Commands.Process.dataFiles opts)
  case mergedData of
    Left err -> print err
    Right jv -> do
      case compileRuleFile jv opts of
        Left err -> print err
        Right s -> do
          eRule <- compiledRule (mkLazyFile "" "" "") s
          case eRule of
            Left err -> print err
            Right r -> do
              processRule (ruleFile opts) (Commands.Process.targetDirectory opts) (dryRun opts) (quiet gopts)
  where
    compiledRule lf (CompileSuccess get) = do
      let (warnings, template) = get lf
      Control.Monad.unless (quiet gopts) $ printWarnings (Commands.Process.suppressWarnings opts) warnings
      pure . YAML.decodeEither' . encodeUtf8 . value $ template

compileRuleFile :: YAML.Value -> ProcessOptions -> IO (Either CompileError CompileSuccess)
compileRuleFile v opt = do
  lazyRf <- readLazy ("/" </> ruleFile opt) (ruleFile opt)
  let job = CompileJob v $ Set.singleton lazyRf
  pure $ compile job

type DryRun = Bool

type Quiet = Bool

processRule :: Rule -> Directory -> DryRun -> Quiet -> IO ()
processRule r d True _ = do
  allTargets <- listAllRecursive $ unDirectory d
  let res = process $ ProcessJob d r allTargets
  case res of
    Left err -> putStrLn $ getMessage err
    Right cs -> putStrLn $ unlines $ numberedLines $ prettyPrintQualifiedCommand <$> cmds cs
  where
    numberedLines = Prelude.zipWith (\n s -> show n ++ "| " ++ s) [(1 :: Int) ..]
processRule r d False q = do
  allTargets <- listAllRecursive $ unDirectory d
  let res = process $ ProcessJob d r allTargets
  case res of
    Left err -> putStrLn $ getMessage err
    Right cs -> do
      runCmds (unDirectory d) (environment r) $ cmds cs

-- getExecutionFunction :: DryRun -> IO (Command -> IO ())
getExecutionFunction :: ProcessOptions -> ProcessSuccess -> IO ()
getExecutionFunction ProcessOptions {dryRun = True} cs = putStrLn $ unlines $ numberedLines $ prettyPrintQualifiedCommand <$> cmds cs
  where
    numberedLines = Prelude.zipWith (\n s -> show n ++ "| " ++ s) [(1 :: Int) ..]
getExecutionFunction ProcessOptions {dryRun = False, Commands.Process.targetDirectory = d, ruleFile = r} cs = runCmds (unDirectory d) (environment r) $ cmds cs

listAllRecursive :: FilePath -> IO [FilePath]
listAllRecursive path = do
  fs <- listFiles path
  ds <- listDirectories path
  rest <- concatMapM listAllRecursive ds
  return $ fs ++ (addTrailingPathSeparator <$> ds) ++ rest
