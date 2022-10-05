{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Tasty.Types.MarkdownTable where

import Data.Bool (bool)
import Data.List (sort)
import Numeric (showFFloat)
import qualified Test.Tasty.Types.JSONOutcome as JSONOutcome
import Test.Tasty.Types.JSONResult (JSONResult (JSONResult))
import Test.Tasty.Types.JSONTestPath (JSONTestPath)
import Test.Tasty.Types.JSONTestResult (JSONTestResult (JSONTestResult))
import Test.Tasty.Types.JSONTestSuiteResult (JSONTestSuiteResult)
import qualified Test.Tasty.Types.JSONTestSuiteResult as JSONTestSuiteResult

newtype MarkdownTable = MarkdownTable [MarkdownTableRow]

data MarkdownTableRow = MarkdownTableRow
  { path :: JSONTestPath,
    success :: Bool,
    durationInSeconds :: Double
  }
  deriving (Eq, Ord)

succeeded :: Bool -> String
succeeded = bool "❌" "✅"

roundFloat :: RealFloat a => a -> String
roundFloat = flip (showFFloat (Just 6)) ""

instance Show MarkdownTableRow where
  show :: MarkdownTableRow -> String
  show MarkdownTableRow {path, success, durationInSeconds} =
    "| " ++ show path ++ " | " ++ succeeded success ++ " | " ++ roundFloat durationInSeconds ++ " |"

instance Show MarkdownTable where
  show :: MarkdownTable -> String
  show (MarkdownTable rows) =
    let header = "| Path | Success | Duration (s) |"
        separator = "| --- | --- | --- |"
     in unlines $ header : separator : sort (map show rows)

fromJSONTestSuiteResult :: JSONTestSuiteResult -> MarkdownTable
fromJSONTestSuiteResult jtr = MarkdownTable rows
  where
    mkRow (JSONTestResult path (JSONResult outcome _ _ durationInSeconds)) = case outcome of
      JSONOutcome.Success -> MarkdownTableRow path True durationInSeconds
      JSONOutcome.Failure _ -> MarkdownTableRow path False durationInSeconds
    rows = map mkRow (JSONTestSuiteResult.results jtr)
