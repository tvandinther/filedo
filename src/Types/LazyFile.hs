module Types.LazyFile (
    LazyFile(..),
    readLazy
) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Functor ( (<&>) )

data LazyFile = LazyFile
    { templateFile :: FilePath 
    , content :: ByteString }

readLazy :: FilePath -> IO LazyFile
readLazy fp = BSL.readFile fp <&> LazyFile fp
