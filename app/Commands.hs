module Commands (
    Command(..)
) where

import Commands.MergeData (MergeDataOptions)
import Commands.Compile (CompileOptions)
import Commands.Process (ProcessOptions)

data Command
    = MergeData MergeDataOptions
    | Compile CompileOptions
    | Process ProcessOptions