module Process (tests) where

import Actions.Process (ProcessError (..), ProcessJob (..), ProcessSuccess (..), process)
import Data.Map qualified as Map
import Test.HUnit
import Types.Rule (Rule (..))

tests :: Test
tests =
  TestList
    [ TestLabel "Test 1" emptyJobTest
    ]

emptyJobTest :: Test
emptyJobTest = TestCase $ do
  let job = emptyJob
  let result = process job
  case result of
    Left err -> assertFailure $ "Expected success, got error: " ++ show err
    Right (ProcessSuccess cmds) -> assertBool "Expected no commands" $ null cmds

emptyJob :: ProcessJob
emptyJob =
  ProcessJob
    { targetDirectory = "",
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