module Types.FileScoped (
    FileScoped(..),
    showPretty,
    findFile
) where
import Data.List (find)

data FileScoped a = FileScoped
    { source :: FilePath
    , value :: a
    } deriving (Show)

instance Functor FileScoped where
    fmap f (FileScoped s v) = FileScoped s (f v)

showPretty :: Show a => FileScoped a -> String
showPretty (FileScoped s v) = s ++ ": " ++ show v

findFile :: FilePath -> [FileScoped a] -> Maybe (FileScoped a)
findFile fp = find ((== fp) . source)
