{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Tasty.Types.JSONFailureReason where

import Control.Exception (Exception (displayException))
import Data.Aeson (ToJSON (toEncoding, toJSON))
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (Encoding, Value)
import Test.Tasty.Aeson.Options (tastyJSONOptions)
import qualified Test.Tasty.Runners as Runners

data JSONFailureReason = TestFailed | TestThrewException String | TestTimedOut Integer | TestDepFailed deriving (Eq, Show)

$(deriveJSON tastyJSONOptions ''JSONFailureReason)

fromFailureReason :: Runners.FailureReason -> JSONFailureReason
fromFailureReason = \case
  Runners.TestFailed -> TestFailed
  Runners.TestThrewException e -> TestThrewException (displayException e)
  Runners.TestTimedOut t -> TestTimedOut t
  Runners.TestDepFailed -> TestDepFailed

instance ToJSON Runners.FailureReason where
  toJSON :: Runners.FailureReason -> Value
  toJSON = toJSON . fromFailureReason
  toEncoding :: Runners.FailureReason -> Encoding
  toEncoding = toEncoding . fromFailureReason
