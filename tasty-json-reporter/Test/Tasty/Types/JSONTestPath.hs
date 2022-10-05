{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
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
  deriving (Eq, Ord)

instance Show JSONTestPath where
  show :: JSONTestPath -> String
  show JSONTestPath {group, name} = foldr (\a b -> a <> "." <> b) name group

$(deriveJSON tastyJSONOptions ''JSONTestPath)
