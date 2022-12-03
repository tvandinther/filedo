module Types.FileScoped (
    FileScoped(..),
    showPretty
) where

data FileScoped a = FileScoped
    { source :: FilePath
    , value :: a
    } deriving (Show)

instance Functor FileScoped where
    fmap f (FileScoped s v) = FileScoped s (f v)

showPretty :: Show a => FileScoped a -> String
showPretty (FileScoped s v) = s ++ ": " ++ show v
