module Actions.Process (
    ProcessJob(..),
    ProcessSuccess(..),
    ProcessError(..),
    process
) where

data ProcessJob = ProcessJob
    { targetDirectory :: String
    , rules :: [String]
    , files :: [String]
    }

data ProcessSuccess = ProcessSuccess
    { placeholder :: String }

newtype ProcessError = ProcessError { getMessage :: String }

process :: ProcessJob -> Either ProcessError ProcessSuccess
process = undefined
