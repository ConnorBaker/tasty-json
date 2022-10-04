{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Test.Tasty.Types.MarkdownTable where

import Data.Bool (bool)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Numeric (showFFloat)
import Test.Tasty.Types.JSONTestPath (JSONTestPath)

newtype MarkdownTable = MarkdownTable (NonEmpty MarkdownTableRow)

data MarkdownTableRow = MarkdownTableRow
  { path :: JSONTestPath,
    success :: Bool,
    durationInSeconds :: Double
  }
  deriving (Eq)

succeeded :: Bool -> String
succeeded = bool "❌" "✅"

roundFloat :: RealFloat a => a -> String
roundFloat = flip (showFFloat (Just 6)) ""

instance Show MarkdownTableRow where
  show :: MarkdownTableRow -> String
  show mtr =
    "| " ++ show mtr.path ++ " | " ++ succeeded mtr.success ++ " | " ++ roundFloat mtr.durationInSeconds ++ " |"

instance Show MarkdownTable where
  show :: MarkdownTable -> String
  show (MarkdownTable rows) =
    let header = "| Path | Success | Duration (s) |"
        separator = "| --- | --- | --- |"
     in unlines $ header : separator : map show (NonEmpty.toList rows)
