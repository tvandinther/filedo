module Types.Command (
    Cmd(..),
    Command(..),
    prettyPrintCmd,
    prettyPrintCommand
) where

import Data.Yaml (FromJSON)
import qualified Data.Yaml as YAML
import qualified Data.Text as T
import Data.Vector ( toList )
import Types.FileScoped (FileScoped(..))

data Cmd = Unscoped Command | Scoped (FileScoped Command) deriving (Show)

newtype Command = Command { unCommand :: [String] }
    deriving (Show)

instance FromJSON Command where
    parseJSON (YAML.String s) = return $ Command [T.unpack s]
    parseJSON (YAML.Array xs) = Command <$> Prelude.mapM YAML.parseJSON (toList xs)
    parseJSON _ = fail "Command must be a string or an array of strings"

isEmpty :: Command -> Bool
isEmpty (Command []) = True
isEmpty _ = False

prettyPrintCmd :: Cmd -> String
prettyPrintCmd (Unscoped c) = prettyPrintCommand c
prettyPrintCmd (Scoped (FileScoped s c)) = s ++ ": " ++ prettyPrintCommand c

prettyPrintCommand :: Command -> String
prettyPrintCommand (Command cs) = unwords cs
