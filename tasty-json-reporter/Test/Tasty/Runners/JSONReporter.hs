{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Runners.JSONReporter
  ( jsonReporter,
    consoleAndJsonReporter,
  )
where

import qualified Control.Concurrent.STM as STM
import Control.Monad (foldM)
import Data.Aeson (ToJSON (toEncoding), encodeFile, fromEncoding)
import Data.ByteString.Builder (hPutBuilder)
import qualified Data.IntMap.Strict as IM
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Proxy (Proxy (Proxy))
import System.IO (IOMode (WriteMode), withBinaryFile)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Ingredients (Ingredient (TestReporter), composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)
import Test.Tasty.Options (OptionDescription (Option), OptionSet, lookupOption)
import Test.Tasty.Options.JSONMeta (JSONMeta (..))
import Test.Tasty.Options.JSONPath (JSONPath (..))
import Test.Tasty.Providers (IsTest)
import Test.Tasty.Runners (NumThreads (getNumThreads), Result, Status (..), StatusMap, TreeFold (foldGroup, foldSingle), foldTestTree, resultSuccessful, trivialFold)
import Test.Tasty.Types.JSONResult (fromResult)
import Test.Tasty.Types.JSONTestResult (JSONTestResult (..))
import Test.Tasty.Types.JSONTestSuiteResult (JSONTestSuiteResult (..))

awaitResult :: StatusMap -> Int -> IO Result
awaitResult statusMap i = STM.atomically $
  case IM.lookup i statusMap of
    Nothing -> error "Looked up a test with an index that was out of bounds"
    Just tvarStatus ->
      STM.readTVar tvarStatus >>= \case
        Done result -> pure result
        _ -> STM.retry

-- Produces a list of list of tests names, where each list starts with the outermost test group and proceeds inwards, to the innermost test.
fullTestNames :: OptionSet -> TestTree -> [(NonEmpty TestName, TestName)]
fullTestNames optionSet testTree = map splitNames testPath
  where
    fg :: OptionSet -> TestName -> [[TestName]] -> [[TestName]]
    fg _ groupName = map (groupName :)
    fs :: IsTest t => OptionSet -> TestName -> t -> [[TestName]]
    fs _ testName _ = [[testName]]

    testPath :: [[TestName]]
    testPath = foldTestTree trivialFold {foldSingle = fs, foldGroup = fg} optionSet testTree

    splitNames :: [TestName] -> (NonEmpty TestName, TestName)
    splitNames = \case
      [] -> error "Empty list of test names"
      [name] -> error $ "Test " ++ name ++ " has no parent group"
      names -> ((NonEmpty.fromList . init) names, last names)

jsonReporter :: Ingredient
jsonReporter = TestReporter [Option $ Proxy @(Maybe JSONPath), Option $ Proxy @(Maybe JSONMeta)] $ \options tree -> do
  JSONPath path <- lookupOption options
  meta@(JSONMeta _) <- lookupOption options
  Just $ \statusMap -> do
    let numThreads :: Int
        numThreads = getNumThreads $ lookupOption options

        -- The test number in the status map corresponds to the list of test group names to locate that test.
        tests :: [(Int, (NonEmpty TestName, TestName))]
        tests = zip [0 ..] $ fullTestNames options tree

        testCount :: Int
        testCount = length tests

        go :: (Bool, [JSONTestResult]) -> (Int, (NonEmpty TestName, TestName)) -> IO (Bool, [JSONTestResult])
        go (accSuccess, accResults) (idx, (group, name)) = do
          result <- awaitResult statusMap idx
          let !success = resultSuccessful result && accSuccess
          let !results = JSONTestResult group name (fromResult result) : accResults
          pure (success, results)
    (success, results) <- foldM go (True, []) tests

    return $ \time ->
      success <$ do
        encodeFile path $ JSONTestSuiteResult {results, time, success, meta, numThreads, testCount}

consoleAndJsonReporter :: Ingredient
consoleAndJsonReporter = composeReporters consoleTestReporter jsonReporter
