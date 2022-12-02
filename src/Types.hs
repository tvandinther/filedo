module Types (
    DataFileType(..),
    TemplateFile,
    Directory(..),
) where

import Types.LazyFile (LazyFile)

data DataFileType
    = JSON
    | YAML
    deriving (Show)

type TemplateFile = LazyFile

newtype Directory = Directory { unDirectory :: FilePath }
    deriving (Show)
