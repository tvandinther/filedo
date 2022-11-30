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
import System.FilePath (dropExtension, takeFileName, makeRelative)
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
    , templates :: [LazyFile] }

data CompileSuccess = CompileSuccess 
    { renderedTemplates :: [(FilePath, Text)]
    , warnings :: [(FilePath, String)] }
    deriving (Show)

newtype CompileError = CompileError String deriving (Show)

compile :: CompileJob -> Either CompileError CompileSuccess
-- compile = undefined
compile (CompileJob d ts) = Control.Arrow.left (CompileError . errorBundlePretty) $ compile' d ts

compile' :: JSON.Value -> [LazyFile] -> Either (ParseErrorBundle Text Void) CompileSuccess
-- compile' = undefined
compile' _ [] = Right $ CompileSuccess [] []
compile' d (t:ts) = do
    cache <- compileMustacheTexts (t:|ts)
    let names = relativePath <$> ts
    let rendered = fmap (renderSingle d cache) (filepathToPName <$> names)
    return $ CompileSuccess (Prelude.zip names $ snd <$> rendered) (Prelude.zip names $ displayMustacheWarning <$> concatMap fst rendered)

-- TODO: Create a custom FileScopedWarning type which also includes the file path
renderSingle :: JSON.Value -> Template -> PName -> ([MustacheWarning], Text)
renderSingle d cache pname = strict $ renderMustacheW (setActiveTemplate pname) d
    where
        strict (ws, t) = (ws, Data.Text.Lazy.toStrict t)
        setActiveTemplate n = cache { templateActual = n }

-- renderMustacheWCache :: Maybe Template -> Template -> JSON.Value -> ([MustacheWarning], Text)
-- renderMustacheWCache cache template d = strict $ renderMustacheW (addCache template cache) d
--     where
--         strict (ws, t) = (ws, Data.Text.Lazy.toStrict t)
--         addCache t Nothing = t
--         addCache t (Just c) = t <> c

compileTemplate :: LazyFile -> Either (ParseErrorBundle Text Void) Template
compileTemplate (LazyFile fp rfp bsl) = compileMustacheText (PName $ Data.Text.pack rfp) (decodeUtf8 $ Data.ByteString.toStrict bsl)

compileMustacheTexts :: NonEmpty LazyFile -> Either (ParseErrorBundle Text Void) Template
compileMustacheTexts (x :| xs) = compileTemplate x >>= \t -> foldrM compileMerge t xs
    where
        compileMerge lf tmp = (<>) <$> compileTemplate lf <*> pure tmp

-- compileCache :: NonEmpty LazyFile -> Either (ParseErrorBundle Text Void) Template
-- compileCache (x:|xs) = compileMustacheTexts (x :| xs)

filepathToPName :: FilePath -> PName
filepathToPName = PName . Data.Text.pack

-- compileMustacheTextsCache' :: NonEmpty LazyFile -> IO (Either (ParseErrorBundle Text Void) Template)
-- compileMustacheTextsCache' (x :| _) = do
--     files <- listFilesRecursive $ takeDirectory $ templateFile x
--     lfs <- mapM readLazy files
--     return $ compileMustacheTexts' lfs