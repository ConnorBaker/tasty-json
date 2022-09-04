{-# LANGUAGE TemplateHaskell #-}

module Test.Tasty.Types.JSONTestSuiteResult where

import Data.Aeson.TH (deriveJSON)
import Test.Tasty.Aeson.Options (tastyJSONOptions)
import Test.Tasty.Options.JSONMeta (JSONMeta)
import Test.Tasty.Runners (Time)
import Test.Tasty.Types.JSONTestResult (JSONTestResult)

data JSONTestSuiteResult = JSONTestSuiteResult {results :: [JSONTestResult], time :: Time, success :: Bool, numThreads :: Int, testCount :: Int, meta :: JSONMeta} deriving (Show)

$(deriveJSON tastyJSONOptions ''JSONTestSuiteResult)
