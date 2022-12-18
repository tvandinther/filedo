module Main (main) where

import Commands (Command (..))
import Options (GlobalOptions (..), optsParser)
import Options.Applicative (execParser)
import Runners.Compile (runCompile)
import Runners.MergeData (runMergeData)
import Runners.Process (runProcess)

main :: IO ()
main = do
  opts <- execParser optsParser
  run opts (optCommand opts)

run :: GlobalOptions -> Command -> IO ()
run _ (MergeData o) = runMergeData o
run _ (Compile o) = runCompile o
run g (Process o) = runProcess g o
