{-# LANGUAGE LambdaCase #-}
module Runners.MergeData (
    runMergeData,
    sendMergeJob,
    sendMergeJob'
) where

import Actions.MergeData

import Commands.MergeData (MergeDataOptions(..))
import Types (DataFileType)
import Utility (processOutput)
import qualified Control.Arrow
import Data.Text
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as JSON

runMergeData :: MergeDataOptions -> IO ()
runMergeData (MergeDataOptions dfs o t) = sendMergeJob t dfs >>= \case
    Left (MergeDataError err) -> putStrLn err
    Right bs -> processOutput o bs

sendMergeJob :: DataFileType -> [FilePath] -> IO (Either MergeDataError Text)
sendMergeJob t dfs = do
    result <- sendJob mergeData t dfs
    return $ Control.Arrow.right mergedData result

sendMergeJob' :: DataFileType -> [FilePath] -> IO (Either MergeDataError JSON.Value)
sendMergeJob' = sendJob mergeData'

sendJob :: (MergeDataJob -> b) -> DataFileType -> [FilePath] -> IO b
sendJob f t dfs = do
    texts <- mapM TIO.readFile dfs
    let result = f $ MergeDataJob texts t
    return result
