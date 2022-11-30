{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Actions.Compile (
    CompileJob(..),
    CompileSuccess(..),
    CompileError(..),
    compile
) where

import Data.Text ( Text, pack )

import Text.Mustache (compileMustacheText, renderMustacheW)
import Data.Foldable (foldrM)
import Data.List.NonEmpty
import qualified Data.Aeson.Types as JSON
import Data.Void
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Mustache.Type
import Text.Megaparsec (errorBundlePretty)
import Data.Text.Lazy (toStrict)
import Data.ByteString (toStrict)
import Types.LazyFile (LazyFile(..))
import System.FilePath (dropExtension)
import Data.Text.Encoding (decodeUtf8)

data CompileJob = CompileJob
    { templateData :: JSON.Value
    , templates :: NonEmpty LazyFile }

newtype CompileSuccess = CompileSuccess Text deriving (Show)
newtype CompileError = CompileError String deriving (Show)

compile :: CompileJob -> Either CompileError CompileSuccess
compile (CompileJob d ts) = do
    let compiled = compileMustacheTexts ts
    case compiled of
        Left err -> Left $ CompileError $ errorBundlePretty err
        Right ms -> do
            let (warnings, rendered) = renderMustacheW ms d
            return $ CompileSuccess $ Data.Text.Lazy.toStrict rendered

compileMustacheTexts :: NonEmpty LazyFile -> Either (ParseErrorBundle Text Void) Template
-- compileMustacheTexts = undefined
compileMustacheTexts (x :| xs) = compileTemplate x >>= \t -> foldrM compileMerge t xs
    where
        compileTemplate (LazyFile fp bsl) = compileMustacheText (PName $ Data.Text.pack $ dropExtension fp) (decodeUtf8 $ Data.ByteString.toStrict bsl)
        compileMerge lf tmp = (<>) <$> compileTemplate lf <*> pure tmp
