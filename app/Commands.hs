module Commands (
    Command(..)
) where

import Commands.MergeData (MergeDataOptions)
import Commands.Compile (CompileOptions)

data Command
    = MergeData MergeDataOptions
    | Compile CompileOptions