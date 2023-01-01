module Shell
  ( runCmds,
    Environment,
  )
where

import Control.Monad qualified
import Data.Map (Map, assocs)
import Data.Set qualified as Set
import GHC.IO.Handle (Handle)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process.Extra
  ( CmdSpec (RawCommand),
    CreateProcess (..),
    ProcessHandle,
    StdStream (Inherit),
    createProcess,
    waitForProcess,
  )
import Types (FileScoped (..))
import Types.Command (Cmd (..))
import Types.Rule (Command (..))

type Environment = Map String String

runCmds :: FilePath -> Environment -> [Cmd] -> IO ()
runCmds _ _ [] = pure ()
runCmds wd env (c : cs) = do
  exitCode <- runCmd wd env c
  Control.Monad.when (isSuccess exitCode) $ runCmds wd env cs

-- runCmdsAsync :: FilePath -> Environment -> [Cmd] -> IO ()
-- runCmdsAsync _ _ [] = pure ()
-- runCmdsAsync wd env cs = do
--   xs <- mapM (runCmdAsync wd env) cs

--   pure ()

isSuccess :: ExitCode -> Bool
isSuccess = (==) ExitSuccess

-- waitForAllProcesses :: [ProcessHandle] -> [ExitCode]
-- waitForAllProcesses ps = loop $ Set.fromList ps
--   where
--     loop xs
--       | null xs = pure ()
--       | otherwise = do
--           (x, _i) <- waitAny (Set.toList xs)
--           loop (Set.delete x xs)

runCmd :: FilePath -> Environment -> Cmd -> IO ExitCode
runCmd wd env cmd = do
  (_, _, _, p) <-
    createProcess $ toProcess wd env cmd
  -- runCmdAsync wd env cmd
  waitForProcess p

runCmdAsync :: FilePath -> Environment -> Cmd -> IO (Handle, Handle, Handle, ProcessHandle)
runCmdAsync wd env cmd = do
  (Just stdIn, Just stdOut, Just stdErr, p) <-
    createProcess $ toProcess wd env cmd
  pure (stdIn, stdOut, stdErr, p)

toProcess :: FilePath -> Environment -> Cmd -> CreateProcess
toProcess wd env (Unscoped (Command c)) =
  defaultProcess
    { cmdspec = RawCommand (head c) (tail c),
      cwd = Just wd,
      env = Just (assocs env)
    }
toProcess wd env (Scoped (FileScoped _ (Command c))) =
  defaultProcess
    { cmdspec = RawCommand (head c) (tail c),
      cwd = Just wd,
      env = Just (assocs env)
    }

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
