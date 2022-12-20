module Commands
  ( Command (..),
  )
where

import Commands.Compile (CompileOptions)
import Commands.MergeData (MergeDataOptions)
import Commands.Process (ProcessOptions)

data Command
  = MergeData MergeDataOptions
  | Compile CompileOptions
  | Process ProcessOptions
