module Actions.Process
  ( ProcessJob (..),
    ProcessSuccess (..),
    ProcessError (..),
    process,
    expandCommand,
  )
where

import Data.List.Extra (sortOn)
import Data.Ord (Down (..))
import System.FilePath ((</>))
import System.FilePath.Glob (compile, decompile, match, simplify)
import Types.Command (Cmd (..))
import Types.FileScoped (FileScoped (..))
import Types.Rule (Command (..), GlobPattern, Rule (..))

data ProcessJob = ProcessJob
  { targetDirectory :: String,
    rule :: Rule,
    files :: [FilePath]
  }

newtype ProcessSuccess = ProcessSuccess
  {cmds :: [Cmd]}
  deriving (Show)

newtype ProcessError = ProcessError {getMessage :: String} deriving (Show)

process :: ProcessJob -> Either ProcessError ProcessSuccess
process job = Right $ ProcessSuccess $ processRule (files job) (rule job)

processRule :: [FilePath] -> Rule -> [Cmd]
processRule fps r = processRule' fps (targets r) r

processRule' :: [FilePath] -> [GlobPattern] -> Rule -> [Cmd]
processRule' _ _ (Rule {skip = True}) = []
processRule' fs rootPatterns r =
  allCommands
    ++ concatMap
      (processRule' fs newRoots)
      (prioritisedSubrules r)
  where
    prioritisedSubrules r' = sortOn (Down . priority) $ rules r'
    newRoots = cartesianConcatGlobs rootPatterns (targets r)
    allCommands = addPreHook r $ addPostHook r $ Scoped <$> expandedCommands
    expandedCommands = expandCommand rootPatterns fs r

addPreHook :: Rule -> [Cmd] -> [Cmd]
addPreHook (Rule {pre = Command []}) = id
addPreHook (Rule {pre = hook}) = (:) $ Unscoped hook

addPostHook :: Rule -> [Cmd] -> [Cmd]
addPostHook (Rule {post = Command []}) = id
addPostHook (Rule {post = hook}) = flip (++) [Unscoped hook]

expandCommand :: [GlobPattern] -> [FilePath] -> Rule -> [FileScoped Command]
expandCommand _ _ (Rule {command = Command []}) = []
expandCommand rootPatterns fs (Rule {command = cmd, targets = tInclude, exclude = tExclude}) =
  flip FileScoped cmd <$> filteredFiles
  where
    filteredFiles = filter (\fp -> isIncludedFile fp && not (isExcludedFile fp)) fs
    isIncludedFile fp = any (`match` fp) (cartesianConcatGlobs rootPatterns tInclude)
    isExcludedFile fp = any (`match` fp) (cartesianConcatGlobs rootPatterns tExclude)

cartesianConcatGlobs :: [GlobPattern] -> [GlobPattern] -> [GlobPattern]
cartesianConcatGlobs [] _ = []
cartesianConcatGlobs _ [] = []
cartesianConcatGlobs roots subs =
  simplify . compile
    <$> [root </> sub | root <- decompile <$> roots, sub <- decompile <$> subs]
