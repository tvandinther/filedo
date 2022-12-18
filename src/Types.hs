module Types
  ( DataFileType (..),
    TemplateFile,
    Directory (..),
    FileScoped (..),
  )
where

import Types.FileScoped (FileScoped (..))
import Types.LazyFile (LazyFile (..))

data DataFileType
  = JSON
  | YAML
  deriving (Show)

type TemplateFile = LazyFile

newtype Directory = Directory {unDirectory :: FilePath}
  deriving (Show)
