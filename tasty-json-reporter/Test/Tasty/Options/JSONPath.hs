{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Test.Tasty.Options.JSONPath where

import Data.Tagged (Tagged (Tagged))
import Test.Tasty.Options (IsOption (defaultValue, optionHelp, optionName, parseValue))

newtype JSONPath = JSONPath FilePath deriving (Eq, Show)

instance IsOption (Maybe JSONPath) where
  defaultValue :: Maybe JSONPath
  defaultValue = Nothing
  parseValue :: String -> Maybe (Maybe JSONPath)
  parseValue = Just . Just . JSONPath
  optionName :: Tagged (Maybe JSONPath) String
  optionName = Tagged "json-path"
  optionHelp :: Tagged (Maybe JSONPath) String
  optionHelp = Tagged "A file path to store the JSON-formatted test results"
