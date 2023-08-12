module Shell
  ( runCmds,
    Environment,
  )
where

import Control.Monad qualified
import Data.Map (Map, assocs)
import GHC.IO.Handle (Handle)
import System.Exit (ExitCode (..))
import System.FilePath (dropTrailingPathSeparator, takeBaseName, takeDirectory, takeExtension)
import System.Process.Extra
  ( CmdSpec (RawCommand),
    CreateProcess (..),
    ProcessHandle,
    StdStream (Inherit),
    createProcess,
    waitForProcess,
  )
import Types (FileScoped (..))
import Types.Command (QualifiedCommand (..))

type Environment = Map String String

runCmds :: FilePath -> Environment -> [QualifiedCommand] -> IO ()
runCmds _ _ [] = pure ()
runCmds wd env (c : cs) = do
  exitCode <- runCmd wd env c
  Control.Monad.when (isSuccess exitCode) $ runCmds wd env cs

-- TODO: Allow failure to be ignored. Change occurs at this function

runCmdsAsync :: FilePath -> Environment -> [QualifiedCommand] -> IO ()
runCmdsAsync _ _ [] = pure ()
runCmdsAsync wd env cs = do
  xs <- mapM (runCmdAsync wd env) cs
  exitCodes <- mapM (waitForProcess . (\(_, _, _, p) -> p)) xs
  pure ()

isSuccess :: ExitCode -> Bool
isSuccess = (==) ExitSuccess

runCmd :: FilePath -> Environment -> QualifiedCommand -> IO ExitCode
runCmd wd env cmd = do
  (_, _, _, p) <-
    createProcess $ toProcess wd env cmd
  waitForProcess p

runCmdAsync :: FilePath -> Environment -> QualifiedCommand -> IO (Handle, Handle, Handle, ProcessHandle)
runCmdAsync wd env cmd = do
  (Just stdIn, Just stdOut, Just stdErr, p) <-
    createProcess $ toProcess wd env cmd
  pure (stdIn, stdOut, stdErr, p)

toProcess :: FilePath -> Environment -> QualifiedCommand -> CreateProcess
toProcess wd env (Unscoped c) =
  defaultProcess
    { cmdspec = c,
      cwd = Just wd,
      env = Just (assocs env)
    }
toProcess wd env (Scoped (FileScoped p c)) =
  defaultProcess
    { cmdspec = c,
      cwd = Just wd,
      env = Just $ assocs env ++ createFileVariables p
    }

createFileVariables :: FilePath -> [(String, String)]
createFileVariables p =
  let path = dropTrailingPathSeparator p
   in [ ("FILEPATH", path),
        ("FILENAME", takeBaseName path),
        ("FILEEXT", tail $ takeExtension path),
        ("FILEDIR", takeDirectory $ dropTrailingPathSeparator path)
      ]

defaultProcess :: CreateProcess
defaultProcess =
  CreateProcess
    { cmdspec = RawCommand "" [],
      cwd = Nothing,
      env = Nothing,
      std_in = Inherit,
      std_out = Inherit,
      std_err = Inherit,
      close_fds = False,
      create_group = False,
      delegate_ctlc = False,
      detach_console = False,
      create_new_console = False,
      new_session = False,
      child_group = Nothing,
      child_user = Nothing,
      use_process_jobs = False
    }
