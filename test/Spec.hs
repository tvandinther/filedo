import Process
import Test.HUnit

main :: IO ()
main = runTestTTAndExit allTests

allTests :: Test
allTests = TestList [TestLabel "Process" Process.tests]
