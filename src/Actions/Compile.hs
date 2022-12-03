{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Actions.Compile (
    CompileJob(..),
    CompileSuccess(..),
    CompileError(..),
    compile
) where

import Data.Text ( Text, pack, unpack )

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
import Types ( FileScoped(..) )
import Control.Arrow ((|||), (&&&))
import Data.Bifunctor (Bifunctor(bimap))

data CompileJob = CompileJob
    { templateData :: JSON.Value
    , templates :: [LazyFile] }

data CompileSuccess = CompileSuccess 
    { renderedTemplates :: [FileScoped Text]
    , warnings :: [FileScoped String] }
    deriving (Show)

newtype CompileError = CompileError {getMessage :: FileScoped String} deriving (Show)

type ParseError = ParseErrorBundle Text Void

compile :: CompileJob -> Either CompileError CompileSuccess
compile (CompileJob _ []) = Right $ CompileSuccess [] []
compile (CompileJob d (t:ts)) = bimap mkError mkSuccess doJob
    where
        mkSuccess r = CompileSuccess (snd <$> r) $ concatMap fst r
        mkError = CompileError . fmap errorBundlePretty
        doJob = compileMustacheTexts (t:|ts) >>= \cache -> do
            return $ fmap (renderSingle d cache) (filepathToPName . relativePath <$> ts)

renderSingle :: JSON.Value -> Template -> PName -> ([FileScoped String], FileScoped Text)
renderSingle d cache pname = 
    fileScope $ strict $ renderMustacheW (setActiveTemplate pname) d
    where
        scope = pNameToFilePath pname
        fileScope (ws, t) = (FileScoped scope . displayMustacheWarning <$> ws, FileScoped scope t)
        strict (ws, t) = (ws, Data.Text.Lazy.toStrict t)
        setActiveTemplate n = cache { templateActual = n }

compileMustacheTexts :: NonEmpty LazyFile -> Either (FileScoped ParseError) Template
compileMustacheTexts (x :| xs) = compileTemplate x >>= \t -> foldrM compileMerge t xs
    where
        compileMerge lf tmp = (<>) <$> compileTemplate lf <*> pure tmp

compileTemplate :: LazyFile -> Either (FileScoped ParseError) Template
compileTemplate (LazyFile fp rfp bsl) = Control.Arrow.left (FileScoped fp) $ compileMustacheText (PName $ Data.Text.pack rfp) (decodeUtf8 $ Data.ByteString.toStrict bsl)

filepathToPName :: FilePath -> PName
filepathToPName = PName . Data.Text.pack

pNameToFilePath :: PName -> FilePath
pNameToFilePath = Data.Text.unpack . unPName
