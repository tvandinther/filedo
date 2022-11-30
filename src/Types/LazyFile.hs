module Types.LazyFile (
    LazyFile(..),
    readLazy
) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Functor ( (<&>) )
import System.FilePath (makeRelative)

data LazyFile = LazyFile
    { path :: FilePath
    , relativePath :: FilePath
    , content :: ByteString }

readLazy :: FilePath -> FilePath -> IO LazyFile
readLazy root fp = BSL.readFile fp <&> LazyFile fp (makeRelative root fp)
