{-# LANGUAGE TemplateHaskell #-}

module Test.Tasty.Types.JSONTestResult where

import Data.Aeson.TH (deriveJSON)
import Test.Tasty (TestName)
import Test.Tasty.Aeson.Options (tastyJSONOptions)
import Test.Tasty.Types.JSONResult (JSONResult)

data JSONTestResult = JSONTestResult {path :: [TestName], result :: JSONResult} deriving (Eq, Show)

$(deriveJSON tastyJSONOptions ''JSONTestResult)
