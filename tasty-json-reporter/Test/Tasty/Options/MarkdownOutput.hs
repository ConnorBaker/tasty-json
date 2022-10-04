{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Test.Tasty.Options.MarkdownOutput where

import Data.Tagged (Tagged (Tagged))
import Test.Tasty.Options (IsOption (defaultValue, optionHelp, optionName, parseValue))

newtype MarkdownOutput = MarkdownOutput Bool deriving (Eq, Show)

instance IsOption (Maybe MarkdownOutput) where
  defaultValue :: Maybe MarkdownOutput
  defaultValue = Just $ MarkdownOutput False
  parseValue :: String -> Maybe (Maybe MarkdownOutput)
  parseValue "true" = Just $ Just $ MarkdownOutput True
  parseValue _ = Just $ Just $ MarkdownOutput False
  optionName :: Tagged (Maybe MarkdownOutput) String
  optionName = Tagged "markdown-output"
  optionHelp :: Tagged (Maybe MarkdownOutput) String
  optionHelp = Tagged "Whether or not to echo markdown output"
