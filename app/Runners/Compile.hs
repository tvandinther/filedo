module Runners.Compile (
    runCompile
) where

import Commands.Compile ( CompileOptions(..) )
import Actions.MergeData ( MergeDataError(errorMessage) )
import Runners.MergeData( sendMergeJob' )
import Types ( DataFileType(JSON), Directory(unDirectory), FileScoped(..) )
import qualified Control.Arrow
import Actions.Compile ( CompileError(..), compile, CompileSuccess(..), CompileJob(..) )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import Types.LazyFile ( readLazy )
import System.Directory.Extra ( listFilesRecursive, createDirectoryIfMissing )
import System.FilePath ( (</>) )
import System.FilePath.Posix ( takeDirectory )
import Types.FileScoped ( showPretty )
-- import qualified Data.Aeson as Yaml
import Data.Yaml ( decodeEither' )
import qualified Data.Yaml as Yaml
import Control.Applicative ((<|>))
import Data.Yaml.Aeson (prettyPrintParseException)

type TargetDirectory = Directory
type OutputDirectory = Directory
type SuppressWarnings = Bool

runCompile :: CompileOptions -> IO ()
runCompile (CompileOptions dfs td od w) = do
    df <- getData dfs
    case df of
        Left err -> putStrLn err
        Right d -> do
            files <- listFilesRecursive $ unDirectory td
            templates <- mapM (readLazy $ unDirectory td) files
            let result = compile $ CompileJob d templates
            case result of
                Left err -> printError err
                Right (CompileSuccess rts ws) -> processOutput w od ws rts

getData :: [FilePath] -> IO (Either String JSON.Value)
getData [] = BS.getContents >>= \bs -> return $ Control.Arrow.left prettyPrintParseException $ Yaml.decodeEither' bs
getData dfs = do
    result <- sendMergeJob' JSON dfs
    return $ Control.Arrow.left errorMessage result

printError :: CompileError -> IO ()
printError (CompileError (FileScoped f e)) = putStrLn $ Prelude.concat ["Error in ", f, " ", e]

processOutput :: SuppressWarnings -> OutputDirectory -> [FileScoped String] -> [FileScoped Text] -> IO ()
processOutput w od ws ts = do
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
