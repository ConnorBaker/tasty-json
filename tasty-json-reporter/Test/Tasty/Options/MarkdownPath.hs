{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Test.Tasty.Options.MarkdownPath where

import Data.Tagged (Tagged (Tagged))
import Test.Tasty.Options (IsOption (defaultValue, optionHelp, optionName, parseValue))

newtype MarkdownPath = MarkdownPath FilePath deriving (Eq, Show)

instance IsOption (Maybe MarkdownPath) where
  defaultValue :: Maybe MarkdownPath
  defaultValue = Nothing
  parseValue :: String -> Maybe (Maybe MarkdownPath)
  parseValue = Just . Just . MarkdownPath
  optionName :: Tagged (Maybe MarkdownPath) String
  optionName = Tagged "markdown-path"
  optionHelp :: Tagged (Maybe MarkdownPath) String
  optionHelp = Tagged "A file path to store the markdown-formatted test results"
