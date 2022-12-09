module Actions.Process (
    ProcessJob(..),
    ProcessSuccess(..),
    ProcessError(..),
    process,
    expandCommand
) where
import Types.Rule ( Rule(..), Command(..), GlobPattern )
import qualified Types.Rule as R
import Types.Command (Cmd(..))
import Types.FileScoped (FileScoped(..))
import System.FilePath.Glob (match, decompile, compile, simplify)
import Data.List.Extra (sortOn)
import qualified Data.Set as Set
import System.FilePath ((</>))
import qualified Debug.Trace as Debug

data ProcessJob = ProcessJob
    { targetDirectory :: String
    , rule :: Rule
    , files :: [FilePath]
    }

data ProcessSuccess = ProcessSuccess
    { placeholder :: [Cmd] }
    deriving (Show)

newtype ProcessError = ProcessError { getMessage :: String } deriving (Show)

process :: ProcessJob -> Either ProcessError ProcessSuccess
process job = Right $ ProcessSuccess $ processRule (files job) (rule job)

processRule :: [FilePath] -> Rule -> [Cmd]
processRule _ (Rule{skip=True}) = []
processRule fps r = filter (const True) allCommands ++ concatMap (processRule fps) (sortOn priority $ R.rules r)
    where
        expandedCommands = expandCommand (decompile $ head $ targets r) fps r
        allCommands = addPreHook r $ addPostHook r $ (Scoped <$> expandedCommands)

addPreHook :: Rule -> [Cmd] -> [Cmd]
addPreHook (Rule{pre=Command []}) = id
addPreHook (Rule{pre=hook}) = (:) $ Unscoped hook

addPostHook :: Rule -> [Cmd] -> [Cmd]
addPostHook (Rule{post=Command []}) = id
addPostHook (Rule{post=hook}) = flip (++) [Unscoped hook]


-- expandCommand :: [FilePath] -> Command -> [GlobPattern] -> [FileScoped Command]
-- expandCommand _ (Rule{command=Command []}) = const []
-- expandCommand files cmd = concatMap $ \pattern -> flip FileScoped cmd <$> filter (match pattern) files

expandCommand :: FilePath -> [FilePath] -> Rule -> [FileScoped Command]
expandCommand _ _ (Rule{command=Command []}) = []
expandCommand root fs (Rule{command=cmd,targets=tInclude,exclude=tExclude}) = 
    flip FileScoped cmd <$> files
    where
        files = filter (\fp -> isIncludedFile fp && not (isExcludedFile fp)) fs
        isIncludedFile fp = any (`match` fp) ( simplify . compile . (root </>) . decompile <$> tInclude)
        isExcludedFile fp = any (`match` fp) ( simplify . compile . (root </>) . decompile <$> tExclude)
