module Main (main) where

import Options.Applicative
import Commands( Command(..) )
import Options ( Opts(..), optsParser )
import Runners.MergeData (runMergeData)


main :: IO ()
main = do
    opts <- execParser optsParser
    case optCommand opts of
        MergeData o -> runMergeData o
        Compile o -> print o
    -- putStrLn ("verbose: " ++ show (verbose opts))
