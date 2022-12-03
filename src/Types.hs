module Types (
    DataFileType(..),
    TemplateFile,
    Directory(..),
    FileScoped(..),
) where

import Types.LazyFile (LazyFile(..))
import Types.FileScoped (FileScoped(..))

data DataFileType
    = JSON
    | YAML
    deriving (Show)

type TemplateFile = LazyFile

newtype Directory = Directory { unDirectory :: FilePath }
    deriving (Show)
