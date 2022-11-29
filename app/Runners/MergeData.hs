{-# LANGUAGE LambdaCase #-}
module Runners.MergeData (
    runMergeData,
    sendMergeJob
) where

import qualified Data.ByteString as BS
import Actions.MergeData

import Commands.MergeData (MergeDataOptions(..))
import Types (DataFileType)
import Data.ByteString (ByteString)
import Utility (processOutput)
import qualified Control.Arrow

runMergeData :: MergeDataOptions -> IO ()
runMergeData (MergeDataOptions dfs o t) = sendMergeJob t dfs >>= \case
    Left (MergeDataError err) -> print err
    Right bs -> processOutput o bs

sendMergeJob :: DataFileType -> [FilePath] -> IO (Either MergeDataError ByteString)
sendMergeJob t dfs = do
    byteStrings <- mapM BS.readFile dfs
    let result = mergeData $ MergeDataJob byteStrings t
    return $ Control.Arrow.right mergedData result
