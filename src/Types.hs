module Types (
    DataFileType(..),
    TemplateFile
) where

import Types.LazyFile (LazyFile)

data DataFileType
    = JSON
    | YAML
    deriving (Show)

type TemplateFile = LazyFile
