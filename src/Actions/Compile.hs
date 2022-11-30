{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Actions.Compile (
    CompileJob(..),
    CompileSuccess(..),
    CompileError(..),
    compile
) where

import Data.Text ( Text, pack )

import Text.Mustache (compileMustacheText, renderMustacheW, renderMustache)
import Data.Foldable (foldrM)
import Data.List.NonEmpty
import qualified Data.Aeson.Types as JSON
import Data.Void
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Mustache.Type
import Text.Megaparsec (errorBundlePretty)
import Data.Text.Lazy (toStrict)
import Data.ByteString (toStrict)
import Types.LazyFile (LazyFile(..), readLazy)
import System.FilePath (dropExtension, takeFileName)
import Data.Text.Encoding (decodeUtf8)
import Text.Mustache.Compile (compileMustacheDir')
import System.FilePath.Posix (takeDirectory)
import Control.Monad (forM_, foldM)
import Text.Mustache (compileMustacheFile)
import System.Directory.Extra (listFilesRecursive)
import qualified Data.Text.Lazy as LT
import qualified Data.List.NonEmpty as NE
import qualified Control.Arrow

data CompileJob = CompileJob
    { templateData :: JSON.Value
    , templates :: NonEmpty LazyFile
    , cache :: [LazyFile] }

data CompileSuccess = CompileSuccess 
    { renderedTemplates :: NonEmpty Text
    , warnings :: [String] }
    deriving (Show)

newtype CompileError = CompileError String deriving (Show)

compile :: CompileJob -> Either CompileError CompileSuccess
compile job = Control.Arrow.left (CompileError . errorBundlePretty) $ compile' job

compile' :: CompileJob -> Either (ParseErrorBundle Text Void) CompileSuccess
compile' (CompileJob d ts cache) = do
    tmpCache <- compileCache cache
    compileds <- mapM (renderSingle d tmpCache) ts
    return $ CompileSuccess (NE.map snd compileds) (displayMustacheWarning <$> concatMap fst compileds)

renderSingle :: JSON.Value -> Maybe Template -> LazyFile -> Either (ParseErrorBundle Text Void) ([MustacheWarning], Text)
renderSingle d cache lf = do 
    tmp <- compileTemplate lf
    return $ renderMustacheWCache cache tmp d

renderMustacheWCache :: Maybe Template -> Template -> JSON.Value -> ([MustacheWarning], Text)
renderMustacheWCache cache template d = strict $ renderMustacheW (addCache template cache) d
    where
        strict (ws, t) = (ws, Data.Text.Lazy.toStrict t)
        addCache t Nothing = t
        addCache t (Just c) = t <> c

compileTemplate :: LazyFile -> Either (ParseErrorBundle Text Void) Template
compileTemplate (LazyFile fp bsl) = compileMustacheText (PName $ Data.Text.pack $ takeFileName fp) (decodeUtf8 $ Data.ByteString.toStrict bsl)

compileMustacheTexts :: NonEmpty LazyFile -> Either (ParseErrorBundle Text Void) Template
compileMustacheTexts (x :| xs) = compileTemplate x >>= \t -> foldrM compileMerge t xs
    where
        compileMerge lf tmp = (<>) <$> compileTemplate lf <*> pure tmp

compileCache :: [LazyFile] -> Either (ParseErrorBundle Text Void) (Maybe Template)
compileCache [] = Right Nothing
compileCache (x:xs) = Control.Arrow.right Just $ compileMustacheTexts (x :| xs)

-- compileMustacheTextsCache' :: NonEmpty LazyFile -> IO (Either (ParseErrorBundle Text Void) Template)
-- compileMustacheTextsCache' (x :| _) = do
--     files <- listFilesRecursive $ takeDirectory $ templateFile x
--     lfs <- mapM readLazy files
--     return $ compileMustacheTexts' lfs