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
import Actions.Compile ( CompileError(..), compile, CompileSuccess(..), CompileJob(..) )
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

type TargetDirectory = Directory
type OutputDirectory = Directory
type SuppressWarnings = Bool

runCompile :: CompileOptions -> IO ()
runCompile (CompileOptions dfs td od w) = getData dfs >>= either putStrLn go
    where
        go :: Yaml.Value -> IO ()
        go d = createJob td d >>= either printErrors (processOutput w od) . compile

runCompile' :: CompileOptions -> IO (Either CompileError CompileSuccess)
runCompile' (CompileOptions dfs td _ _) = getData dfs >>= either (pure . Left . mkCompileError) go
    where
        mkCompileError s = CompileError [FileScoped "" s]
        go :: Yaml.Value -> IO (Either CompileError (Maybe CompileSuccess))
        go d = do
            mb <- runMaybeT $ createJob td d
            case mb of
                Nothing -> pure . Right $ Nothing
                Just a -> pure $ Control.Arrow.right Just $ compile a

createJob :: TargetDirectory -> Yaml.Value -> MaybeT IO CompileJob
createJob td d = MaybeT $ do
    files <- listFilesRecursive $ unDirectory td
    templates <- mapM (readLazy $ unDirectory td) files
    pure $ mkJob templates
    where
        mkJob [] = Nothing
        mkJob (t:ts) = Just . CompileJob d $ t:|ts

getData :: [FilePath] -> IO (Either String JSON.Value)
getData [] = BS.getContents >>= \bs -> return $ Control.Arrow.left prettyPrintParseException $ Yaml.decodeEither' bs
getData dfs = do
    result <- sendJob mergeData' JSON dfs
    return $ Control.Arrow.left errorMessage result

printErrors :: CompileError -> IO ()
printErrors (CompileError es) = putStrLn $ Prelude.concatMap showError es
    where
        showError (FileScoped f e) = Prelude.concat ["Error in ", f, " ", e]

processOutput :: SuppressWarnings -> OutputDirectory -> CompileSuccess -> IO ()
processOutput w od (CompileSuccess ws ts) = printWarnings w ws >> mapM_ processFile ts
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

-- challenge: given `newtype M a = M (IO (Maybe a))`, write its `Functor`, `Applicative`, and `Monad` instances

-- newtype M a = M { runM :: M a -> IO (Maybe a)}
newtype M a = M { runM :: IO (Maybe a) }

instance Functor M where
    fmap :: (a -> b) -> M a -> M b
    fmap f (M a) = M $ (fmap . fmap) f a

instance Applicative M where
    pure :: a -> M a
    pure = M . pure . Just
    (<*>) :: M (a -> b) -> M a -> M b
    (<*>) (M f) (M a) = M $ do
        f' <- f
        a' <- a
        pure $ f' <*> a'

instance Monad M where
    (>>=) :: M a -> (a -> M b) -> M b
    (>>=) (M a) f = M $ do
        a' <- a
        case a' of
            Nothing -> pure Nothing
    --         Just a'' -> runM $ f a''    
    -- (>>=) (M a) f = M $ do
    --     -- maybe pure (a -> b) (Maybe a)
    --     a' <- a
    --     case a' of
    --         Nothing -> pure Nothing
            Just a'' -> let (M b) = f a'' in b
