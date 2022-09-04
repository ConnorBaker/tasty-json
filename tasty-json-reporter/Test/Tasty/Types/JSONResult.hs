{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Tasty.Types.JSONResult where

import Data.Aeson (ToJSON (toEncoding, toJSON))
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (Encoding, Value)
import Test.Tasty.Aeson.Options (tastyJSONOptions)
import Test.Tasty.Runners (Time)
import qualified Test.Tasty.Runners as Runners
import Test.Tasty.Types.JSONOutcome (JSONOutcome, fromOutcome)

data JSONResult = JSONResult
  { outcome :: JSONOutcome,
    description :: String,
    shortDescription :: String,
    time :: Time
  }
  deriving (Show, Eq)

$(deriveJSON tastyJSONOptions ''JSONResult)

fromResult :: Runners.Result -> JSONResult
fromResult Runners.Result {..} =
  JSONResult
    { outcome = fromOutcome resultOutcome,
      description = resultDescription,
      shortDescription = resultShortDescription,
      time = resultTime
    }

instance ToJSON Runners.Result where
  toJSON :: Runners.Result -> Value
  toJSON = toJSON . fromResult
  toEncoding :: Runners.Result -> Encoding
  toEncoding = toEncoding . fromResult
