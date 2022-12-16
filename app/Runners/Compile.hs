module Runners.Compile (
    runCompile,
    runCompile',
    createJob,
    getData
) where

import Commands.Compile ( CompileOptions(..) )
import Actions.MergeData ( MergeDataError(errorMessage), mergeData' )
import Runners.MergeData( sendJob )
import Types ( DataFileType(JSON), Directory(unDirectory), FileScoped(..) )
import Control.Arrow ( ArrowChoice(left, right) )
import Actions.Compile ( CompileError(..), compile, CompileSuccess(..), CompileJob(..), CompileResult )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import Data.List.NonEmpty as NE ( NonEmpty((:|)), fromList )
import Types.LazyFile ( readLazy )
import System.Directory.Extra ( listFilesRecursive, createDirectoryIfMissing )
import System.FilePath ( (</>) )
import System.FilePath.Posix ( takeDirectory )
import Types.FileScoped ( showPretty )
import qualified Data.Yaml as Yaml
import Data.Yaml.Aeson ( prettyPrintParseException )
import Data.Functor ((<&>))
import GHC.Base (IO(..))
import Control.Monad.Trans.Maybe ( MaybeT(..), runMaybeT )
import Control.Monad((>=>))
import qualified Data.Set as Set
import qualified Data.Set as Data

type TargetDirectory = Directory
type OutputDirectory = Directory
type SuppressWarnings = Bool

runCompile :: CompileOptions -> IO ()
runCompile (CompileOptions dfs td od w) = getData dfs >>= either putStrLn go
    where
        go :: Yaml.Value -> IO ()
        go d = do
            mbJob <- runMaybeT $ createJob td d 
            case mbJob of 
                Nothing -> putStrLn "No templates found."
                Just job -> either printErrors (processOutput w od . getAllResults job) $ compile job
        getAllResults :: CompileJob -> CompileSuccess -> [CompileResult]
        getAllResults j s = fmap (getResult s) (Data.toList $ templates' j)

runCompile' :: CompileOptions -> IO (Either CompileError CompileSuccess)
runCompile' (CompileOptions dfs td _ _) = getData dfs >>= either (pure . Left . mkCompileError) go
    where
        mkCompileError s = CompileError [FileScoped "" s]
        go d = do
            mbJob <- runMaybeT $ createJob td d
            case mbJob of
                Nothing -> pure . Left . mkCompileError $ "No templates found"
                Just job -> pure . compile $ job

createJob :: TargetDirectory -> Yaml.Value -> MaybeT IO CompileJob
createJob td d = MaybeT $ do
    files <- listFilesRecursive $ unDirectory td
    templates <- mapM (readLazy $ unDirectory td) files
    pure $ mkJob templates
    where
        mkJob [] = Nothing
        mkJob ts = Just . CompileJob d $ Set.fromList ts

getData :: [FilePath] -> IO (Either String JSON.Value)
getData [] = BS.getContents >>= \bs -> return $ Control.Arrow.left prettyPrintParseException $ Yaml.decodeEither' bs
getData dfs = do
    result <- sendJob mergeData' JSON dfs
    return $ Control.Arrow.left errorMessage result

printErrors :: CompileError -> IO ()
printErrors (CompileError es) = putStrLn $ Prelude.concatMap showError es
    where
        showError (FileScoped f e) = Prelude.concat ["Error in ", f, " ", e]

processOutput :: SuppressWarnings -> OutputDirectory -> [CompileResult] -> IO ()
processOutput w od rs = printWarnings w (concat ws) >> mapM_ processFile ts
    where
        (ws, ts) = unzip rs
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
