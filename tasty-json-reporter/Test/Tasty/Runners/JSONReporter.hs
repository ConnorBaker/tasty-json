{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Runners.JSONReporter
  ( jsonReporter,
    consoleAndJsonReporter,
  )
where

import qualified Control.Concurrent.STM as STM
import Control.Monad (foldM)
import Data.Aeson (ToJSON (toEncoding), fromEncoding)
import Data.ByteString.Builder (hPutBuilder)
import qualified Data.IntMap.Strict as IM
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
fullTestNames :: OptionSet -> TestTree -> [[TestName]]
fullTestNames = foldTestTree trivialFold {foldSingle = fs, foldGroup = fg}
  where
    fg :: OptionSet -> TestName -> [[TestName]] -> [[TestName]]
    fg _ groupName = map (groupName :)
    fs :: IsTest t => OptionSet -> TestName -> t -> [[TestName]]
    fs _ testName _ = [[testName]]

jsonReporter :: Ingredient
jsonReporter = TestReporter [Option $ Proxy @(Maybe JSONPath), Option $ Proxy @(Maybe JSONMeta)] $ \options tree -> do
  JSONPath path <- lookupOption options
  meta@(JSONMeta _) <- lookupOption options
  Just $ \statusMap -> do
    let numThreads :: Int
        numThreads = getNumThreads $ lookupOption options

        -- The test number in the status map corresponds to the list of test group names to locate that test.
        tests :: [(Int, [TestName])]
        tests = zip [0 ..] $ fullTestNames options tree

        testCount :: Int
        testCount = length tests

        go :: (Bool, [JSONTestResult]) -> (Int, [TestName]) -> IO (Bool, [JSONTestResult])
        go (accSuccess, accResults) (idx, fullTestName) = do
          result <- awaitResult statusMap idx
          let !success = resultSuccessful result && accSuccess
          let !results = JSONTestResult fullTestName (fromResult result) : accResults
          pure (success, results)
    (success, results) <- foldM go (True, []) tests

    return $ \time ->
      success <$ do
        withBinaryFile path WriteMode $ \handle ->
          hPutBuilder handle $
            fromEncoding $
              toEncoding $
                JSONTestSuiteResult {..}

consoleAndJsonReporter :: Ingredient
consoleAndJsonReporter = composeReporters consoleTestReporter jsonReporter
