module Utility (
    processOutput
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

processOutput :: Maybe FilePath -> ByteString -> IO ()
processOutput Nothing bs = BS.putStrLn bs
processOutput (Just path) bs = do
    BS.writeFile path bs
    print $ "Output written to: " ++ show path