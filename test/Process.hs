module Process (tests) where

import Actions.Process (ProcessError (..), ProcessJob (..), ProcessSuccess (..), process)
import Data.Map qualified as Map
import System.FilePath.Glob (compile)
import System.Process.Extra (CmdSpec (ShellCommand))
import Test.HUnit
import Types (Directory (Directory))
import Types.Rule (Rule (..))

tests :: Test
tests =
  TestList
    [ TestLabel "Empty job" emptyJobTest,
      TestLabel "Command expansion" commandExpansionTest
    ]

emptyJobTest :: Test
emptyJobTest = TestCase $ do
  let job = emptyJob
  let result = process job
  case result of
    Left err -> assertFailure $ "Expected success, got error: " ++ show err
    Right (ProcessSuccess cmds) -> assertBool "Expected no commands" $ null cmds

commandExpansionTest :: Test
commandExpansionTest = TestCase $ do
  let job =
        emptyJob
          { rule =
              emptyRule
                { command = Just $ ShellCommand "cat $FILEPATH",
                  targets = compile <$> ["*"]
                },
            files = ["foo.txt", "bar.zip"]
          }
  let result = process job
  case result of
    Left err -> assertFailure $ "Expected success, got error: " ++ show err
    Right (ProcessSuccess cmds) -> assertEqual "Expected two commands" 2 (length cmds)

emptyJob :: ProcessJob
emptyJob =
  ProcessJob
    { targetDirectory = Directory "",
      rule = emptyRule,
      files = []
    }

emptyRule :: Rule
emptyRule =
  Rule
    { priority = 0,
      skip = False,
      targets = [],
      exclude = [],
      useStdIn = False,
      parallelise = False,
      ignoreErrors = False,
      pre = Nothing,
      command = Nothing,
      post = Nothing,
      environment = Map.empty,
      rules = []
    }