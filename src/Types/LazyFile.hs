module Types.LazyFile
  ( LazyFile (..),
    readLazy,
    mkLazyFile,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Functor ((<&>))
import System.FilePath (makeRelative)

data LazyFile = LazyFile
  { path :: FilePath,
    relativePath :: FilePath,
    content :: ByteString
  }
  deriving (Show, Eq, Ord)

readLazy :: FilePath -> FilePath -> IO LazyFile
readLazy root fp = BSL.readFile fp <&> LazyFile fp (makeRelative root fp)

mkLazyFile :: FilePath -> FilePath -> ByteString -> LazyFile
mkLazyFile root fp = LazyFile fp (makeRelative root fp)
