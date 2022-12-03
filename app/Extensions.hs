module Extensions (
    directory,
    someNE
) where

import Options.Applicative
import System.FilePath (isValid)
import Types ( Directory(..) )
import Data.List.NonEmpty ( NonEmpty((:|)) )
-- import qualified Data.List.NonEmpty as NE

directory :: ReadM Directory
directory = str >>= \s -> if isValid s then return $ Directory s else readerError "Invalid directory."

someNE :: Alternative f => f a -> f (NonEmpty a)
-- someNE = fmap NE.fromList . some -- unsafe (but still safe) implementation
someNE = liftA2 (:|) <*> many
