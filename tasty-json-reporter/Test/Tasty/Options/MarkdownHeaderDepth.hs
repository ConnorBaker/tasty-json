{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Test.Tasty.Options.MarkdownHeaderDepth where

import Data.Bool (bool)
import Data.Tagged (Tagged (Tagged))
import Numeric.Natural (Natural)
import Test.Tasty.Options (IsOption (defaultValue, optionHelp, optionName, parseValue), safeRead)

newtype MarkdownHeaderDepth = MarkdownHeaderDepth Natural deriving (Eq, Show)

instance IsOption (Maybe MarkdownHeaderDepth) where
  defaultValue :: Maybe MarkdownHeaderDepth
  defaultValue = Just (MarkdownHeaderDepth 4)
  parseValue :: String -> Maybe (Maybe MarkdownHeaderDepth)
  parseValue = fmap (\v -> bool Nothing (Just $ MarkdownHeaderDepth v) (1 <= v && v <= 6)) . safeRead
  optionName :: Tagged (Maybe MarkdownHeaderDepth) String
  optionName = Tagged "markdown-header-depth"
  optionHelp :: Tagged (Maybe MarkdownHeaderDepth) String
  optionHelp = Tagged "The maximum number of sub-headers to create for each test group"
