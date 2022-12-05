{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Types.Rule (
    Rule(..)
) where

import Data.Map ( Map )
import Data.Yaml (FromJSON)
import qualified Data.Yaml as YAML
import Data.Aeson ((.:?), (.!=))
import System.FilePath.Glob ( Pattern, compile )
import qualified Data.Text as T
import Data.Vector

newtype Command = Command { unCommand :: [String] }
    deriving (Show)

instance FromJSON Command where
    parseJSON (YAML.String s) = return $ Command [T.unpack s]
    parseJSON (YAML.Array xs) = Command <$> Prelude.mapM YAML.parseJSON (Data.Vector.toList xs)
    parseJSON _ = fail "Command must be a string or an array of strings"

type GlobPattern = Pattern

instance FromJSON Pattern where
    parseJSON (YAML.String s) = return $ compile (T.unpack s)
    -- parseJSON (YAML.Array xs) = compile <$> Prelude.mapM YAML.parseJSON (Data.Vector.toList xs)
    parseJSON _ = fail "Glob pattern must be a string"

data Rule = Rule 
    { priority :: Integer
    , skip :: Bool
    , targets :: [GlobPattern]
    , exclude :: [GlobPattern]
    , useStdIn :: Bool
    , parallelise :: Bool
    , ignoreErrors :: Bool
    , pre :: Command
    , command :: Command
    , post :: Command
    , environment :: Map String String
    , rules :: [Rule] }
    deriving (Show)

instance FromJSON Rule where
    parseJSON (YAML.Object o) = Rule 
        <$> o .:? "priority" .!= 0
        <*> o .:? "skip" .!= False
        <*> o .:? "targets" .!= ["*"]
        <*> o .:? "exclude" .!= []
        <*> o .:? "useStdIn" .!= False
        <*> o .:? "parallelise" .!= False
        <*> o .:? "ignoreErrors" .!= False
        <*> o .:? "pre" .!= Command []
        <*> o .:? "command" .!= Command []
        <*> o .:? "post" .!= Command []
        <*> o .:? "environment" .!= mempty
        <*> o .:? "rules" .!= []
    parseJSON _ = fail "Expected Object for Rule"
