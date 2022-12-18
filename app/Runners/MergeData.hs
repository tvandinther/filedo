module Runners.MergeData
  ( runMergeData,
    sendJob,
  )
where

import Actions.MergeData
  ( MergeDataError (errorMessage),
    MergeDataJob (MergeDataJob),
    MergeDataSuccess (MergeDataSuccess),
    mergeData,
  )
import Commands.MergeData (MergeDataOptions (..))
import qualified Data.Text.IO as TIO
import Types (DataFileType)

runMergeData :: MergeDataOptions -> IO ()
runMergeData (MergeDataOptions dfs o t) = sendJob mergeData t dfs >>= either (putStrLn . errorMessage) (processOutput o)

sendJob :: (MergeDataJob -> b) -> DataFileType -> [FilePath] -> IO b
sendJob f t dfs = f . MergeDataJob t <$> mapM TIO.readFile dfs

processOutput :: Maybe FilePath -> MergeDataSuccess -> IO ()
processOutput Nothing (MergeDataSuccess txt) = TIO.putStrLn txt
processOutput (Just path) (MergeDataSuccess txt) = do
  TIO.writeFile path txt
  print $ "Output written to: " ++ show path
