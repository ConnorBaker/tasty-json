{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Tasty.Types.JSONTestPath where

import Data.Aeson.TH (deriveJSON)
import Data.List.NonEmpty (NonEmpty)
import Test.Tasty (TestName)
import Test.Tasty.Aeson.Options (tastyJSONOptions)

data JSONTestPath = JSONTestPath
  { group :: NonEmpty TestName,
    name :: TestName
  }
  deriving (Eq)

instance Show JSONTestPath where
  show :: JSONTestPath -> String
  show jtp = foldr (\a b -> a <> "." <> b) jtp.name jtp.group

$(deriveJSON tastyJSONOptions ''JSONTestPath)
