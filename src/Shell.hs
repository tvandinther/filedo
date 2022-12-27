-- module Shell
--   ( runCmds,
--     Environment,
--   )
-- where

-- import Data.Map (Map, assocs)
-- import System.Process.Extra
--   ( CmdSpec (RawCommand),
--     CreateProcess (..),
--     StdStream (Inherit),
--     createProcess,
--     waitForProcess,
--   )
-- import Types.Command (Cmd)
-- import System.Command

-- type Environment = Map String String

-- runCmds :: FilePath -> Environment -> [Cmd] -> IO ()
-- runCmds _ _ [] = pure ()
-- runCmds wd env cs = iterate (->>) success cs

-- runCmd :: FilePath -> Environment -> Cmd -> IO ()
-- runCmd wd env cmd = do
--   let cs = unCommand cmd
--   (Just stdIn, Just stdOut, Just stdErr, p) <-
--     createProcess $
--       CreateProcess
--         { cmdspec = RawCommand (head cs) (tail cs),
--           cwd = Just wd,
--           env = Just (assocs env),
--           std_in = Inherit,
--           std_out = Inherit,
--           std_err = Inherit,
--           close_fds = False,
--           create_group = False
--         }
--   exitCode <- waitForProcess p
--   pure ()