module Types (
    DataFileType(..)
) where

data DataFileType
    = JSON
    | YAML
    deriving (Show)
