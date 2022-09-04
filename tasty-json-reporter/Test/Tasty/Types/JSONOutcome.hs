{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Tasty.Types.JSONOutcome where

import Data.Aeson (ToJSON (toEncoding, toJSON))
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (Encoding, Value)
import Test.Tasty.Aeson.Options (tastyJSONOptions)
import qualified Test.Tasty.Runners as Runners
import Test.Tasty.Types.JSONFailureReason (JSONFailureReason, fromFailureReason)

data JSONOutcome = Success | Failure JSONFailureReason deriving (Eq, Show)

$(deriveJSON tastyJSONOptions ''JSONOutcome)

fromOutcome :: Runners.Outcome -> JSONOutcome
fromOutcome = \case
  Runners.Success -> Success
  Runners.Failure failureReason -> Failure (fromFailureReason failureReason)

instance ToJSON Runners.Outcome where
  toJSON :: Runners.Outcome -> Value
  toJSON = toJSON . fromOutcome
  toEncoding :: Runners.Outcome -> Encoding
  toEncoding = toEncoding . fromOutcome
