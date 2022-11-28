module Actions.Compile (
    CompileJob(..),
    CompileSuccess(..),
    CompileError(..),
    runCompile
) where
import Data.ByteString (ByteString)

data CompileJob = CompileJob
    { templateData :: ByteString 
    , template :: ByteString }

newtype CompileSuccess = CompileSuccess ByteString deriving (Show)
newtype CompileError = CompileError String deriving (Show)

runCompile :: CompileJob -> Either CompileError CompileSuccess
runCompile = undefined
