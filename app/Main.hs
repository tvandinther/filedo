-- {-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Lib
import Data.Semigroup ((<>))
import Options.Applicative
import Commands( Command(..) )
import Commands.MergeData
import Commands.Compile
import Options ( Opts(..), optsParser )


main :: IO ()
main = do
    opts <- execParser optsParser
    case optCommand opts of
        MergeData x -> print x
        Compile x -> print x
    putStrLn ("verbose: " ++ show (verbose opts))
