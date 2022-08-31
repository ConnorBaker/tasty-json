{-# LANGUAGE TemplateHaskell #-}

module Test.Tasty.Types.JSONTestResult where

import Data.Aeson.TH (deriveJSON)
import Test.Tasty (TestName)
import Test.Tasty.Aeson.Options (myOptions)
import Test.Tasty.Aeson.Orphans ()
import Test.Tasty.Runners (Result)

data JSONTestResult = JSONTestResult {path :: [TestName], result :: Result}

$(deriveJSON myOptions ''JSONTestResult)