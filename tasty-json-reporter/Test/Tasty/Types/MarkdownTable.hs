{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Tasty.Types.MarkdownTable where

import Data.Bool (bool)
import Data.Function (on)
import Data.List (genericReplicate, groupBy, partition, sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import GHC.IO (unsafePerformIO)
import Numeric (showFFloat)
import Numeric.Natural (Natural)
import Test.Tasty (TestName)
import qualified Test.Tasty.Types.JSONOutcome as JSONOutcome
import Test.Tasty.Types.JSONResult (JSONResult (JSONResult))
import Test.Tasty.Types.JSONTestPath (JSONTestPath (..))
import Test.Tasty.Types.JSONTestResult (JSONTestResult (JSONTestResult))
import Test.Tasty.Types.JSONTestSuiteResult (JSONTestSuiteResult)
import qualified Test.Tasty.Types.JSONTestSuiteResult as JSONTestSuiteResult

data MarkdownTable = MarkdownTable
  { markdownHeaderDepth :: Natural,
    rows :: [MarkdownTableRow]
  }

data MarkdownTableRow = MarkdownTableRow
  { path :: NonEmpty TestName,
    success :: Bool,
    durationInSeconds :: Double
  }
  deriving (Eq, Ord)

instance Show MarkdownTableRow where
  show MarkdownTableRow {path, success, durationInSeconds} =
    "| " <> pathString <> " | " <> succeeded success <> " | " <> roundFloat durationInSeconds <> " |"
    where
      pathString = foldr1 (\a b -> a <> "." <> b) path

succeeded :: Bool -> String
succeeded = bool "❌" "✅"

roundFloat :: RealFloat a => a -> String
roundFloat = flip (showFFloat (Just 6)) mempty

roundPercentage :: RealFloat a => a -> String
roundPercentage = (<> "%") . flip (showFFloat (Just 2)) mempty . (* 100.0)

groupByPath :: [MarkdownTableRow] -> [(TestName, [MarkdownTableRow])]
groupByPath rows = groupSplitOff
  where
    grouped = groupBy ((==) `on` (NonEmpty.head . path)) . sort $ rows
    groupSplitOff = [(NonEmpty.head $ path $ head group, map (\row -> row {path = NonEmpty.fromList $ NonEmpty.tail $ path row}) group) | group <- grouped]

successRate :: [MarkdownTableRow] -> Double
successRate rows = fromIntegral (length $ filter success rows) / fromIntegral (length rows)

f ::
  -- | Max header depth
  Natural ->
  -- | Current header depth
  Natural ->
  -- | Rows
  (TestName, [MarkdownTableRow]) ->
  String
f maxHeaderDepth currentHeaderDepth rows
  | maxHeaderDepth == 0 || currentHeaderDepth == 0 = error "Markdown header depth must be at least 1; constructor invariant violated"
  | maxHeaderDepth < currentHeaderDepth = error "Markdown header depth must be at least as large as the current header depth; recursion invariant violated"
f maxHeaderDepth currentHeaderDepth (groupName, tests) = stringified
  where
    -- Bare tests must be printed this go-round, but nestedTests can go another.
    (nestedTests, bareTests) = partition ((> 1) . length . path) tests

    testGroupToLines :: Natural -> (TestName, [MarkdownTableRow]) -> [String]
    testGroupToLines headerDepth (groupName', tests') = stringified
      where
        headerFormat = genericReplicate headerDepth '#'
        headerRow = bool "\n" "" (headerDepth == 1) <> headerFormat <> " " <> groupName'
        successRateRow = "Success rate: " <> roundPercentage (successRate tests)
        durationInSecondsRow = "Duration: " <> roundFloat (sum $ durationInSeconds <$> tests) <> " seconds"

        tableHeader = "| Test | Success | Duration (seconds) |"
        tableSeparator = "| --- | --- | --- |"
        table =
          if null tests'
            then []
            else "\n" <> tableHeader : tableSeparator : fmap show tests'

        stringified = headerRow <> "\n" : successRateRow <> "\n" : durationInSecondsRow : table

    -- If we're on the last header depth, we want to fold nestedTests under the bareTests group.
    stringified =
      concat $
        if currentHeaderDepth < maxHeaderDepth
          then [unlines $ testGroupToLines currentHeaderDepth (groupName, bareTests)] <> map (f maxHeaderDepth (currentHeaderDepth + 1)) (groupByPath nestedTests)
          else [unlines $ testGroupToLines currentHeaderDepth (groupName, bareTests <> nestedTests)]

instance Show MarkdownTable where
  show :: MarkdownTable -> String
  show (MarkdownTable markdownHeaderDepth rows) = f markdownHeaderDepth 1 testRoot
    where
      testRoot = case groupByPath rows of
        [testRoot] -> testRoot
        _ -> error "Markdown table must have exactly one test root; constructor invariant violated"

fromJSONTestSuiteResult :: Natural -> JSONTestSuiteResult -> MarkdownTable
fromJSONTestSuiteResult markdownHeaderDepth jtr = MarkdownTable markdownHeaderDepth rows
  where
    mkRow (JSONTestResult path (JSONResult outcome _ _ durationInSeconds)) = case outcome of
      JSONOutcome.Success -> MarkdownTableRow (group path <> NonEmpty.singleton (name path)) True durationInSeconds
      JSONOutcome.Failure _ -> MarkdownTableRow (group path <> NonEmpty.singleton (name path)) False durationInSeconds
    rows = map mkRow (JSONTestSuiteResult.results jtr)
