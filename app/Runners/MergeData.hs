module Runners.MergeData (
    runMergeData
) where

import qualified Data.ByteString as BS
import Actions.MergeData

import Commands.MergeData (MergeDataOptions(..))
import qualified Commands.MergeData as Opts
import qualified Data.ByteString.Char8 as BS

runMergeData :: MergeDataOptions -> IO ()
runMergeData o = do
    byteStrings <- mapM BS.readFile (Opts.dataFiles o)
    let result = mergeData $ MergeDataJob byteStrings (Opts.outputType o)
    case result of
        Right res -> print "Success" >> processOutput (outputFile o) res
        Left MergeDataError{ errorMessage=em } -> print em

processOutput :: Maybe FilePath -> MergeDataSuccess -> IO ()
processOutput Nothing result = BS.putStrLn $ mergedData result
processOutput (Just path) result = do
    BS.writeFile path (mergedData result)
    print $ "Output written to: " ++ show path