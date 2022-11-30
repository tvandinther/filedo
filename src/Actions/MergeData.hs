module Actions.MergeData (
    MergeDataJob(..),
    MergeDataSuccess(..),
    MergeDataError(..),
    mergeData,
    mergeData'
) where

import Data.ByteString (ByteString)
import qualified Data.Yaml as Yaml

import qualified Data.Aeson as JSON
import Data.Aeson.KeyMap (unionWith)
import Data.Aeson.Types ( Value(..), Object(..) )

import Types (DataFileType(..))
import Data.ByteString.Lazy (toStrict)
import Data.List (foldl1')
import Data.Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

data MergeDataJob = MergeDataJob
    { dataFiles :: [Text]
    , outputType :: DataFileType }
    deriving (Show)

data MergeDataSuccess = MergeDataSuccess { mergedData :: Text } deriving (Show)
data MergeDataError = MergeDataError { errorMessage :: String } deriving (Show)

mergeData :: MergeDataJob -> Either MergeDataError MergeDataSuccess
mergeData MergeDataJob{ dataFiles=dfs, outputType=t } = case mapM Yaml.decodeEither' (encodeUtf8 <$> dfs) of
    Left err -> Left $ MergeDataError { errorMessage = show err }
    Right xs -> Right $ MergeDataSuccess { mergedData = encodeAST t $ mergeASTs xs }

mergeData' :: MergeDataJob -> Either MergeDataError JSON.Value
mergeData' MergeDataJob{ dataFiles=dfs } = case mapM Yaml.decodeEither' (encodeUtf8 <$> dfs) of
    Left err -> Left $ MergeDataError { errorMessage = show err }
    Right xs -> Right $ mergeASTs xs

encodeAST :: DataFileType -> Yaml.Value -> Text
encodeAST JSON = decodeUtf8 . toStrict . JSON.encode
encodeAST YAML = decodeUtf8 . Yaml.encode

mergeASTs :: [JSON.Value] -> JSON.Value
mergeASTs = Data.List.foldl1' mergeAST
    where
        mergeAST :: JSON.Value -> JSON.Value -> JSON.Value
        mergeAST (JSON.Object a) (JSON.Object b) = JSON.Object $ unionWith mergeAST a b
        mergeAST _ b = b