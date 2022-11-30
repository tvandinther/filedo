module Main (main) where

import Options.Applicative ( execParser )
import Commands( Command(..) )
import Options ( Opts(..), optsParser )
import Runners.MergeData (runMergeData)
import Runners.Compile (runCompile)


main :: IO ()
main = do
    opts <- execParser optsParser
    run $ optCommand opts
    -- putStrLn ("verbose: " ++ show (verbose opts))

run :: Command -> IO ()
run (MergeData o) = runMergeData o
run (Compile o) = runCompile o
