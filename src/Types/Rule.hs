{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types.Rule
  ( Rule (..),
    Command (..),
    GlobPattern,
  )
where

import Data.Aeson ((.!=), (.:?))
import Data.Map (Map)
import Data.Text qualified as T
import Data.Yaml (FromJSON)
import Data.Yaml qualified as YAML
import System.FilePath.Glob (Pattern, compile)
import Types.Command (Command (..))

type GlobPattern = Pattern

instance FromJSON Pattern where
  parseJSON (YAML.String s) = return $ compile (T.unpack s)
  parseJSON _ = fail "Glob pattern must be a string"

data Rule = Rule
  { priority :: Integer,
    skip :: Bool,
    targets :: [GlobPattern],
    exclude :: [GlobPattern],
    useStdIn :: Bool,
    parallelise :: Bool,
    ignoreErrors :: Bool,
    pre :: Command,
    command :: Command,
    post :: Command,
    environment :: Map String String,
    rules :: [Rule]
  }
  deriving (Show)

instance FromJSON Rule where
  parseJSON (YAML.Object o) =
    Rule
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
