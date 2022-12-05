module Runners.Process (
    runProcess
) where

import Commands.Process (ProcessOptions(..))
import qualified Data.Yaml as YAML
import Types (Directory(..))
import System.FilePath ((</>))
import Data.Yaml (prettyPrintParseException)
import Types.Rule (Rule)

runProcess :: ProcessOptions -> IO ()
runProcess (ProcessOptions d rf c df w) = do
    file <- YAML.decodeFileEither rf
    case file of
        Left err -> putStrLn $ prettyPrintParseException err
        Right s -> print (s :: Rule)
