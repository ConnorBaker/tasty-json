{-# LANGUAGE TemplateHaskell #-}

module Test.Tasty.Types.JSONTestResult where

import Data.Aeson.TH (deriveJSON)
import Test.Tasty.Aeson.Options (tastyJSONOptions)
import Test.Tasty.Types.JSONResult (JSONResult)
import Test.Tasty.Types.JSONTestPath (JSONTestPath)

data JSONTestResult = JSONTestResult {path :: JSONTestPath, result :: JSONResult} deriving (Eq, Show)

$(deriveJSON tastyJSONOptions ''JSONTestResult)
