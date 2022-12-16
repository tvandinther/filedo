module Main (main) where

import Options.Applicative ( execParser )
import Commands( Command(..) )
import Options ( GlobalOptions(..), optsParser )
import Runners.MergeData (runMergeData)
import Runners.Compile (runCompile)
import Runners.Process (runProcess)


main :: IO ()
main = do
    opts <- execParser optsParser
    run opts (optCommand opts)
    -- putStrLn ("verbose: " ++ show (verbose opts))

run :: GlobalOptions -> Command -> IO ()
run g (MergeData o) = runMergeData o
run g (Compile o) = runCompile o
run g (Process o) = runProcess g o
