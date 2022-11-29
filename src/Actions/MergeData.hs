module Actions.MergeData (
    MergeDataJob(..),
    MergeDataSuccess(..),
    MergeDataError(..),
    mergeData
) where

import Data.ByteString (ByteString)
import qualified Data.Yaml as Yaml

import qualified Data.Aeson as JSON
import Data.Aeson.KeyMap (unionWith)
import Data.Aeson.Types ( Value(..), Object(..) )

import Types (DataFileType(..))
import Data.ByteString.Lazy (toStrict)
import Data.List (foldl1')

data MergeDataJob = MergeDataJob
    { dataFiles :: [ByteString]
    , outputType :: DataFileType }
    deriving (Show)

data MergeDataSuccess = MergeDataSuccess { mergedData :: ByteString } deriving (Show)
data MergeDataError = MergeDataError { errorMessage :: String } deriving (Show)

mergeData :: MergeDataJob -> Either MergeDataError MergeDataSuccess
mergeData MergeDataJob{ dataFiles=dfs, outputType=t } = case mapM Yaml.decodeEither' dfs of
    Left err -> Left $ MergeDataError { errorMessage = show err }
    Right xs -> Right $ MergeDataSuccess { mergedData = encodeAST t $ mergeASTs xs }

encodeAST :: DataFileType -> Yaml.Value -> ByteString
encodeAST JSON = toStrict . JSON.encode
encodeAST YAML = Yaml.encode

mergeASTs :: [JSON.Value] -> JSON.Value
mergeASTs = foldl1' mergeAST
    where
        mergeAST :: JSON.Value -> JSON.Value -> JSON.Value
        mergeAST (JSON.Object a) (JSON.Object b) = JSON.Object $ unionWith mergeAST a b
        mergeAST _ b = b