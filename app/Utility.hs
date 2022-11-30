module Utility (
    processOutput
) where

import Data.Text
import qualified Data.Text.IO as TIO

processOutput :: Maybe FilePath -> Text -> IO ()
processOutput Nothing txt = TIO.putStrLn txt
processOutput (Just path) txt = do
    TIO.writeFile path txt
    print $ "Output written to: " ++ show path