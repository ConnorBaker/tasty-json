{-# LANGUAGE TemplateHaskell #-}

module Test.Tasty.Types.JSONTestResult where

import Data.Aeson.TH (deriveJSON)
import Data.List.NonEmpty (NonEmpty)
import Test.Tasty (TestName)
import Test.Tasty.Aeson.Options (tastyJSONOptions)
import Test.Tasty.Types.JSONResult (JSONResult)

data JSONTestResult = JSONTestResult {group :: NonEmpty TestName, name :: TestName, result :: JSONResult} deriving (Eq, Show)

$(deriveJSON tastyJSONOptions ''JSONTestResult)
