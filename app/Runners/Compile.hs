module Runners.Compile (
    runCompile
) where

import Commands.Compile (CompileOptions(..), TemplateOptions (..))
import Data.ByteString (ByteString, getContents)
import qualified Data.ByteString.Char8 as BS
import Actions.MergeData
import Runners.MergeData(sendMergeJob)
import Types
import qualified Control.Arrow
import Actions.Compile (CompileError)

runCompile :: CompileOptions -> IO ()
runCompile (CompileOptions dfs (SingleTemplate tf o)) = runCompile' dfs [tf] o
runCompile (CompileOptions dfs (MultipleTemplates tfs o)) = runCompile' dfs tfs (Just o)

runCompile' :: [FilePath] -> [FilePath] -> Maybe FilePath -> IO ()
runCompile' dfs tfs o = do
    bs <- sendMergeJob JSON dfs
    case bs of
        Left (MergeDataError err) -> print err
        Right databs -> do
            let result = compileTemplates tfs databs
            case result of
                Left err -> print err
                Right compiledbs -> processOutput o compiledbs

compileTemplates :: [FilePath] -> ByteString -> Either CompileError ByteString
compileTemplates = undefined

compileTemplate :: FilePath -> ByteString -> Either CompileError ByteString
compileTemplate = undefined

compile :: ByteString -> [FilePath] -> IO (Either String ByteString)
compile d = undefined

getDataFile :: [FilePath] -> IO (Either String ByteString)
getDataFile [] = sequence $ Right BS.getContents
getDataFile dfs = do
    result <- sendMergeJob JSON dfs
    return $ Control.Arrow.left errorMessage result

processOutput :: Maybe FilePath -> ByteString -> IO ()
processOutput Nothing bs = BS.putStrLn bs
processOutput (Just path) bs = do
    BS.writeFile path bs
    print $ "Output written to: " ++ show path
