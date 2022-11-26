module Data (
    DataFileType(..)
) where

import Data.Char (toLower)

data DataFileType
    = JSON
    | YAML
    deriving (Show)

-- instance Read DataFileType where
--     readsPrec _ value =
--         tryParse [("json", JSON), ("yaml", YAML)]
--         where
--             tryParse [] = []
--             tryParse ((attempt, result):xs) =
--                 if map toLower value == attempt
--                     then [(result, "")]
--                     else tryParse xs