{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson (eitherDecodeFileStrict')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
-- import Data.HashSet (HashSet)
-- import qualified Data.HashSet as HashSet

import Data.List.NonEmpty (NonEmpty)
import System.Environment (getArgs)
import Test.Tasty (TestName)
import Test.Tasty.Types.JSONResult ()
import qualified Test.Tasty.Types.JSONResult as JSONResult
import qualified Test.Tasty.Types.JSONTestPath as JSONTestPath
import Test.Tasty.Types.JSONTestResult (JSONTestResult)
import qualified Test.Tasty.Types.JSONTestResult as JSONTestResult
import Test.Tasty.Types.JSONTestSuiteResult (JSONTestSuiteResult)
import qualified Test.Tasty.Types.JSONTestSuiteResult as JSONTestSuiteResult

totalTestTime :: JSONTestSuiteResult -> Double
totalTestTime =
  sum
    . fmap (JSONResult.durationInSeconds . JSONTestResult.result)
    . JSONTestSuiteResult.results

totalNumTests :: JSONTestSuiteResult -> Int
totalNumTests = length . JSONTestSuiteResult.results

groupByTestGroupPrefix :: [JSONTestResult] -> HashMap (NonEmpty TestName) [JSONTestResult]
groupByTestGroupPrefix =
  HashMap.fromListWith (<>)
    . fmap (\jtr -> (jtr.path.group, pure jtr))

-- groupedToTable :: HashMap [String] [JSONTestResult] -> Table
-- groupedToTable grouped = HashMap.foldMapWithKey (k -> v -> m) (HashMap k v)
--   where
--     stripGroupPrefix :: [String] -> [JSONTestResult] -> [JSONTestResult]
--     stripGroupPrefix prefix = fmap (\t -> t {JSONTestResult.path = drop (length prefix) (JSONTestResult.path t)})

defaultMain :: FilePath -> IO JSONTestSuiteResult
defaultMain jsonPath =
  eitherDecodeFileStrict' jsonPath >>= \case
    Left err -> error err
    Right (json :: JSONTestSuiteResult) -> pure json

main :: IO ()
main =
  getArgs >>= \case
    [jsonPath] -> do
      jtsr <- defaultMain jsonPath
      putStrLn $ "Total test time: " <> show (totalTestTime jtsr)
      putStrLn $ "Total number of tests: " <> show (totalNumTests jtsr)
      putStrLn $ "Tests grouped by test group prefix:" <> show (groupByTestGroupPrefix $ JSONTestSuiteResult.results jtsr)
    _ -> putStrLn "Usage: tasty-json-markdown <json-path>"
