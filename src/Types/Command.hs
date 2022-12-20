module Types.Command
  ( Cmd (..),
    Command (..),
    printCmd,
    prettyPrintCmd,
    prettyPrintCommand,
  )
where

import Data.Text qualified as T
import Data.Vector (toList)
import Data.Yaml (FromJSON)
import Data.Yaml qualified as YAML
import Types.FileScoped (FileScoped (..))

data Cmd = Unscoped Command | Scoped (FileScoped Command) deriving (Show)

newtype Command = Command {unCommand :: [String]}
  deriving (Show)

instance FromJSON Command where
  parseJSON (YAML.String s) = return $ Command [T.unpack s]
  parseJSON (YAML.Array xs) = Command <$> Prelude.mapM YAML.parseJSON (toList xs)
  parseJSON _ = fail "Command must be a string or an array of strings"

printCmd :: Cmd -> String
printCmd (Unscoped c) = unwords $ unCommand c
printCmd (Scoped (FileScoped _ c)) = unwords (unCommand c)

prettyPrintCmd :: Cmd -> String
prettyPrintCmd (Unscoped c) = prettyPrintCommand c
prettyPrintCmd (Scoped (FileScoped s c)) = s ++ ": " ++ prettyPrintCommand c

prettyPrintCommand :: Command -> String
prettyPrintCommand (Command cs) = unwords cs
