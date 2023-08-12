{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types.Command
  ( QualifiedCommand (..),
    Command,
    printCmd,
    prettyPrintCmd,
    -- prettyPrintCommand,
  )
where

import Data.Text qualified as T
import Data.Vector (toList)
import Data.Yaml (FromJSON)
import Data.Yaml qualified as YAML
import System.Process.Extra (CmdSpec (RawCommand, ShellCommand))
import Types.FileScoped (FileScoped (..))

type Command = CmdSpec

data QualifiedCommand = Unscoped Command | Scoped (FileScoped Command) deriving (Show)

instance FromJSON CmdSpec where
  parseJSON (YAML.Array xs) =
    case mapM (\case YAML.String str -> Just (T.unpack str); _ -> Nothing) (toList xs) of
      Just strs -> pure $ RawCommand (head strs) (tail strs)
      Nothing -> fail "Non-string value found in command array"
  parseJSON (YAML.String s) = pure . ShellCommand $ T.unpack s
  parseJSON _ = fail "Command must be a string or an array of strings"

printCmd :: QualifiedCommand -> String
printCmd (Unscoped c) = show c
printCmd (Scoped (FileScoped _ c)) = show c

prettyPrintCmd :: QualifiedCommand -> String
prettyPrintCmd (Unscoped c) = show c
prettyPrintCmd (Scoped (FileScoped s c)) = s ++ ": " ++ show c
