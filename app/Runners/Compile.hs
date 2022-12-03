module Runners.Compile (
    runCompile
) where

import Commands.Compile ( CompileOptions(..) )
import Actions.MergeData ( MergeDataError(errorMessage), mergeData' )
import Runners.MergeData( sendJob )
import Types ( DataFileType(JSON), Directory(unDirectory), FileScoped(..) )
import qualified Control.Arrow
import Actions.Compile ( CompileError(..), compile, CompileSuccess(..), CompileJob(..) )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import Types.LazyFile ( readLazy )
import System.Directory.Extra ( listFilesRecursive, createDirectoryIfMissing )
import System.FilePath ( (</>) )
import System.FilePath.Posix ( takeDirectory )
import Types.FileScoped ( showPretty )
import qualified Data.Yaml as Yaml
import Data.Yaml.Aeson ( prettyPrintParseException )

type TargetDirectory = Directory
type OutputDirectory = Directory
type SuppressWarnings = Bool

runCompile :: CompileOptions -> IO ()
runCompile (CompileOptions dfs td od w) = getData dfs >>= either putStrLn go
    where
        go d = createJob td d >>= either printError (processOutput w od) . compile

createJob :: TargetDirectory -> Yaml.Value -> IO CompileJob
createJob td d = do
    files <- listFilesRecursive $ unDirectory td
    templates <- mapM (readLazy $ unDirectory td) files
    return $ CompileJob d templates

getData :: [FilePath] -> IO (Either String JSON.Value)
getData [] = BS.getContents >>= \bs -> return $ Control.Arrow.left prettyPrintParseException $ Yaml.decodeEither' bs
getData dfs = do
    result <- sendJob mergeData' JSON dfs
    return $ Control.Arrow.left errorMessage result

printError :: CompileError -> IO ()
printError (CompileError (FileScoped f e)) = putStrLn $ Prelude.concat ["Error in ", f, " ", e]

processOutput :: SuppressWarnings -> OutputDirectory -> CompileSuccess -> IO ()
processOutput w od (CompileSuccess ts ws) = do
    printWarnings w ws
    mapM_ processFile ts
    where
        root = unDirectory od
        processFile (FileScoped p t)
            | T.length t > 0 = do
            let path = root </> p
            createDirectoryIfMissing True $ takeDirectory path
            putStrLn $ "Writing " ++ path ++ "..."
            TIO.writeFile path t
            | otherwise = putStrLn $ "Skipping " ++ p ++ " because it is empty."

printWarnings :: SuppressWarnings -> [FileScoped String] -> IO ()
printWarnings True _ = return ()
printWarnings False ws = do
    putStrLn . showHeader . Prelude.length $ ws
    putStrLn $ Prelude.unlines (showPretty <$> ws)
    where
        showHeader 0 = "There are no warnings."
        showHeader 1 = "There is 1 warning:"
        showHeader n = "There are " ++ show n ++ " warnings:"
