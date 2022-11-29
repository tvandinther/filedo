module Actions.Compile (
    CompileJob(..),
    CompileSuccess(..),
    CompileError(..),
    runCompile
) where

import Data.ByteString (ByteString)

import Data.Aeson( FromJSON )
import qualified Data.Yaml as Yaml

data CompileJob = CompileJob
    { templateData :: Yaml.Value
    , template :: ByteString }

newtype CompileSuccess = CompileSuccess ByteString deriving (Show)
newtype CompileError = CompileError String deriving (Show)

runCompile :: CompileJob -> Either CompileError CompileSuccess
runCompile = undefined
