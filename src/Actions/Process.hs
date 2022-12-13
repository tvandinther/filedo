module Actions.Process (
    ProcessJob(..),
    ProcessSuccess(..),
    ProcessError(..),
    process,
    expandCommand
) where
import Types.Rule ( Rule(..), Command(..), GlobPattern )
import Types.Command (Cmd(..))
import Types.FileScoped (FileScoped(..))
import System.FilePath.Glob (match, decompile, compile, simplify)
import Data.List.Extra (sortOn)
import System.FilePath ((</>))
import Data.Ord (Down(..))

data ProcessJob = ProcessJob
    { targetDirectory :: String
    , rule :: Rule
    , files :: [FilePath]
    }

newtype ProcessSuccess = ProcessSuccess
    { cmds :: [Cmd] }
    deriving (Show)

newtype ProcessError = ProcessError { getMessage :: String } deriving (Show)

process :: ProcessJob -> Either ProcessError ProcessSuccess
process job = Right $ ProcessSuccess $ processRule (files job) (rule job)

processRule :: [FilePath] -> Rule -> [Cmd]
processRule fps r = processRule' fps (targets r) r

processRule' :: [FilePath] -> [GlobPattern] -> Rule -> [Cmd]
processRule' _ _ (Rule{skip=True}) = []
processRule' fs rootPatterns rule = allCommands ++ concatMap 
    (processRule' fs newRoots)
    (prioritisedSubrules rule)
    where
        prioritisedSubrules r = sortOn (Down . priority) $ rules r
        newRoots = cartesianConcatGlobs rootPatterns (targets rule)
        allCommands = addPreHook rule $ addPostHook rule $ Scoped <$> expandedCommands
        expandedCommands = expandCommand rootPatterns fs rule

addPreHook :: Rule -> [Cmd] -> [Cmd]
addPreHook (Rule{pre=Command []}) = id
addPreHook (Rule{pre=hook}) = (:) $ Unscoped hook

addPostHook :: Rule -> [Cmd] -> [Cmd]
addPostHook (Rule{post=Command []}) = id
addPostHook (Rule{post=hook}) = flip (++) [Unscoped hook]

expandCommand :: [GlobPattern] -> [FilePath] -> Rule -> [FileScoped Command]
expandCommand _ _ (Rule{command=Command []}) = []
expandCommand rootPatterns fs (Rule{command=cmd,targets=tInclude,exclude=tExclude}) = 
    flip FileScoped cmd <$> files
    where
        files = filter (\fp -> isIncludedFile fp && not (isExcludedFile fp)) fs
        isIncludedFile fp = any (`match` fp) (cartesianConcatGlobs rootPatterns tInclude)
        isExcludedFile fp = any (`match` fp) (cartesianConcatGlobs rootPatterns tExclude)

cartesianConcatGlobs :: [GlobPattern] -> [GlobPattern] -> [GlobPattern]
cartesianConcatGlobs [] _ = []
cartesianConcatGlobs _ [] = []
cartesianConcatGlobs roots subs = simplify . compile <$> 
    [root </> sub | root <- decompile <$> roots, sub <- decompile <$> subs]
