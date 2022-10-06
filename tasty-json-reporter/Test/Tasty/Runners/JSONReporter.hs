{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Runners.JSONReporter
  ( jsonReporter,
    markdownReporter,
    reporters,
  )
where

import qualified Control.Concurrent.STM as STM
import Control.Monad (foldM)
import Data.Aeson (encodeFile)
import qualified Data.IntMap.Strict as IM
import qualified Data.List.NonEmpty as NonEmpty
import Data.Proxy (Proxy (Proxy))
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Ingredients (Ingredient (TestReporter), composeReporters)
import Test.Tasty.Options (OptionDescription (Option), OptionSet, lookupOption)
import Test.Tasty.Options.JSONMeta (JSONMeta (..))
import Test.Tasty.Options.JSONPath (JSONPath (..))
import Test.Tasty.Options.MarkdownPath (MarkdownPath (..))
import Test.Tasty.Providers (IsTest)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)
import Test.Tasty.Runners (NumThreads (getNumThreads), Result, Status (..), StatusMap, TreeFold (foldGroup, foldSingle), foldTestTree, resultSuccessful, trivialFold)
import Test.Tasty.Types.JSONResult as JSONResult (fromResult)
import Test.Tasty.Types.JSONTestPath as JSONTestPath
  ( JSONTestPath (..),
  )
import Test.Tasty.Types.JSONTestResult as JSONTestResult
  ( JSONTestResult (JSONTestResult),
  )
import Test.Tasty.Types.JSONTestSuiteResult as JSONTestSuiteResult
  ( JSONTestSuiteResult (JSONTestSuiteResult),
  )
import Test.Tasty.Types.MarkdownTable as MarkdownTable (fromJSONTestSuiteResult)

awaitResult :: StatusMap -> Int -> IO Result
awaitResult statusMap i = STM.atomically $
  case IM.lookup i statusMap of
    Nothing -> error "Looked up a test with an index that was out of bounds"
    Just tvarStatus ->
      STM.readTVar tvarStatus >>= \case
        Done result -> pure result
        _ -> STM.retry

-- Produces a list of list of tests names, where each list starts with the outermost test group and proceeds inwards, to the innermost test.
fullTestNames :: OptionSet -> TestTree -> [JSONTestPath]
fullTestNames optionSet testTree = map splitNames testPath
  where
    fg :: OptionSet -> TestName -> [[TestName]] -> [[TestName]]
    fg _ groupName = map (groupName :)
    fs :: IsTest t => OptionSet -> TestName -> t -> [[TestName]]
    fs _ testName _ = [[testName]]

    testPath :: [[TestName]]
    testPath = foldTestTree trivialFold {foldSingle = fs, foldGroup = fg} optionSet testTree

    splitNames :: [TestName] -> JSONTestPath
    splitNames = \case
      [] -> error "Empty list of test names"
      [orphanName] -> error $ "Test " ++ orphanName ++ " has no parent group"
      names -> JSONTestPath {group = (NonEmpty.fromList . init) names, name = last names}

gatherSuccessAndResults :: OptionSet -> TestTree -> StatusMap -> IO (Bool, [JSONTestResult])
gatherSuccessAndResults options tree statusMap = do
  let -- The test number in the status map corresponds to the list of test group names to locate that test.
      tests :: [(Int, JSONTestPath)]
      tests = zip [0 ..] $ fullTestNames options tree

      go :: (Bool, [JSONTestResult]) -> (Int, JSONTestPath) -> IO (Bool, [JSONTestResult])
      go (accSuccess, accResults) (idx, testPath) = do
        result <- awaitResult statusMap idx
        let !success = resultSuccessful result && accSuccess
        let !results = JSONTestResult testPath (fromResult result) : accResults
        pure (success, results)
  foldM go (True, []) tests

-- TODO: There's a lot of code shared here with markdownReporter.
-- It would be nice to factor out the commonalities into a helper function.
jsonReporter :: Ingredient
jsonReporter = TestReporter [Option $ Proxy @(Maybe JSONPath), Option $ Proxy @(Maybe JSONMeta)] $ \options tree -> do
  JSONPath filePath <- lookupOption options
  meta@(JSONMeta _) <- lookupOption options
  let numThreads = getNumThreads $ lookupOption options
  Just $ \statusMap -> do
    (success, results) <- gatherSuccessAndResults options tree statusMap
    let testCount = length results
    pure $ \time -> do
      let jsonRepresentation = JSONTestSuiteResult results time success numThreads testCount meta
      success <$ do
        encodeFile filePath jsonRepresentation

markdownReporter :: Ingredient
markdownReporter = TestReporter [Option $ Proxy @(Maybe MarkdownPath), Option $ Proxy @(Maybe JSONMeta)] $ \options tree -> do
  MarkdownPath filePath <- lookupOption options
  meta@(JSONMeta _) <- lookupOption options
  let numThreads = getNumThreads $ lookupOption options
  Just $ \statusMap -> do
    (success, results) <- gatherSuccessAndResults options tree statusMap
    let testCount = length results
    pure $ \time -> do
      let jsonRepresentation = JSONTestSuiteResult results time success numThreads testCount meta
      success <$ do
        writeFile filePath $ show $ MarkdownTable.fromJSONTestSuiteResult jsonRepresentation

reporters :: Ingredient
reporters = composeReporters consoleTestReporter $ composeReporters markdownReporter jsonReporter
