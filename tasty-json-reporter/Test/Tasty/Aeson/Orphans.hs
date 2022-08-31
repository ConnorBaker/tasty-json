{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Tasty.Aeson.Orphans where

import Control.Exception (SomeException)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding, toJSON), Value (Null))
import Data.Aeson.TH (deriveJSON)
import Test.Tasty.Aeson.Options (myOptions)
import Test.Tasty.Providers.ConsoleFormat (ResultDetailsPrinter)
import Test.Tasty.Runners (FailureReason (..), Outcome (..), Result (..))

instance ToJSON SomeException where
  toJSON = toJSON . show
  toEncoding = toEncoding . show

instance FromJSON SomeException where
  parseJSON = error "Not implemented"

instance ToJSON ResultDetailsPrinter where
  toJSON = const Null
  toEncoding = const (toEncoding Null)

instance FromJSON ResultDetailsPrinter where
  parseJSON = error "Not implemented"

$(deriveJSON myOptions ''FailureReason)
$(deriveJSON myOptions ''Outcome)
$(deriveJSON myOptions ''Result)
