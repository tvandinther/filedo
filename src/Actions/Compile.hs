{-# LANGUAGE LambdaCase #-}
module Actions.Compile (
    CompileJob(..),
    CompileSuccess(..),
    CompileError(..),
    CompileResult,
    compile,
) where

import Data.Text ( Text, pack, unpack )

import Text.Mustache (compileMustacheText, renderMustacheW)
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as NE
import qualified Data.Aeson.Types as JSON
import Data.Void ( Void )
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Mustache.Type
    ( displayMustacheWarning, PName(..), Template(templateActual) )
import Text.Megaparsec (errorBundlePretty)
import Data.Text.Lazy (toStrict)
import Data.ByteString (toStrict)
import Types.LazyFile (LazyFile(..))
import Data.Text.Encoding (decodeUtf8)
import qualified Control.Arrow
import Types.FileScoped ( FileScoped(..) )
import Data.Bifunctor (Bifunctor(bimap))
import Data.Either (partitionEithers)
import Data.Semigroup (Semigroup(sconcat))
import Data.Set (Set, toList)
import qualified Data.Set as Set

data CompileJob = CompileJob
    { templateData' :: JSON.Value
    , templates' :: Set LazyFile }
    deriving (Show)

newtype CompileSuccess = CompileSuccess
    { getResult :: LazyFile -> CompileResult }

type CompileResult = ([FileScoped String], FileScoped Text)

newtype CompileError = CompileError {getMessages :: [FileScoped String]} deriving (Show)

type ParseError = ParseErrorBundle Text Void

compile :: CompileJob -> Either CompileError CompileSuccess
compile (CompileJob d templates) = bimap mkError CompileSuccess (doJob $ Set.toList templates)
    where
        mkError es = CompileError $ fmap errorBundlePretty <$> es
        doJob [] = Right $ const ([], FileScoped "" (pack ""))
        doJob (t:ts) = compileMustacheTexts (t:|ts) >>= \cache -> pure $ renderSingle d cache . (filepathToPName . relativePath)

renderSingle :: JSON.Value -> Template -> PName -> ([FileScoped String], FileScoped Text)
renderSingle d cache pname = fileScope $ strict $ renderMustacheW (setActiveTemplate pname) d
    where
        scope = pNameToFilePath pname
        fileScope (ws, t) = (FileScoped scope . displayMustacheWarning <$> ws, FileScoped scope t)
        strict (ws, t) = (ws, Data.Text.Lazy.toStrict t)
        setActiveTemplate n = cache { templateActual = n }

compileMustacheTexts :: NonEmpty LazyFile -> Either [FileScoped ParseError] Template
compileMustacheTexts (x:|xs) = process . partitionEithers $ compileTemplate <$> x:xs
    where
        process (errs, []) = Left errs
        process (_, t:ts) = Right . sconcat $ (t:|ts)

compileTemplate :: LazyFile -> Either (FileScoped ParseError) Template
compileTemplate (LazyFile fp rfp bsl) = Control.Arrow.left (FileScoped fp) 
    $ compileMustacheText (filepathToPName rfp) (decodeUtf8 $ Data.ByteString.toStrict bsl)

filepathToPName :: FilePath -> PName
filepathToPName = PName . Data.Text.pack

pNameToFilePath :: PName -> FilePath
pNameToFilePath = Data.Text.unpack . unPName
