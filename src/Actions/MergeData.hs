module Actions.MergeData
  ( MergeDataJob (..),
    MergeDataSuccess (..),
    MergeDataError (..),
    mergeData,
    mergeData',
  )
where

import Control.Arrow qualified
import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap (unionWith)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString.Lazy (toStrict)
import Data.List (foldl1')
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Yaml qualified as YAML
import Types (DataFileType (..))

data MergeDataJob = MergeDataJob
  { outputType :: DataFileType,
    dataFiles :: [Text]
  }
  deriving (Show)

newtype MergeDataSuccess = MergeDataSuccess {mergedData :: Text} deriving (Show)

newtype MergeDataError = MergeDataError {errorMessage :: String} deriving (Show)

mergeData :: MergeDataJob -> Either MergeDataError MergeDataSuccess
mergeData job = Control.Arrow.right (MergeDataSuccess . encodeAST (outputType job)) $ mergeData' job

mergeData' :: MergeDataJob -> Either MergeDataError JSON.Value
mergeData' (MergeDataJob _ dfs) = bimap mkError mkSuccess $ mapM YAML.decodeEither' (encodeUtf8 <$> dfs)
  where
    mkError = MergeDataError . YAML.prettyPrintParseException
    mkSuccess = mergeASTs

encodeAST :: DataFileType -> YAML.Value -> Text
encodeAST JSON = decodeUtf8 . toStrict . JSON.encode
encodeAST YAML = decodeUtf8 . YAML.encode

mergeASTs :: [JSON.Value] -> JSON.Value
mergeASTs = Data.List.foldl1' mergeAST
  where
    mergeAST :: JSON.Value -> JSON.Value -> JSON.Value
    mergeAST (JSON.Object a) (JSON.Object b) = JSON.Object $ unionWith mergeAST a b
    mergeAST _ b = b
