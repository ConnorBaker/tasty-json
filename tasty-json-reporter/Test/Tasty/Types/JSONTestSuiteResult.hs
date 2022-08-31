{-# LANGUAGE TemplateHaskell #-}

module Test.Tasty.Types.JSONTestSuiteResult where

import Data.Aeson.TH (deriveJSON)
import Test.Tasty.Aeson.Options (myOptions)
import Test.Tasty.Aeson.Orphans ()
import Test.Tasty.Runners (Time)
import Test.Tasty.Types.JSONTestResult (JSONTestResult)
import Test.Tasty.Options.JSONMeta (JSONMeta)


data JSONTestSuiteResult = JSONTestSuiteResult {results :: [JSONTestResult], time :: Time, success :: Bool, numThreads :: Int, testCount :: Int, meta :: JSONMeta}

$(deriveJSON myOptions ''JSONTestSuiteResult)