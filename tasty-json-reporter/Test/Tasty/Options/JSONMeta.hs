{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Tasty.Options.JSONMeta where

import Data.Aeson.TH (deriveJSON)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Tagged (Tagged (Tagged))
import Data.Text.Lazy (Text, pack, split, unpack)
import Test.Tasty.Aeson.Options (tastyJSONOptions)
import Test.Tasty.Options (IsOption (defaultValue, optionHelp, optionName, parseValue))

newtype JSONMeta = JSONMeta (HashMap String String) deriving (Eq, Show)

$(deriveJSON tastyJSONOptions ''JSONMeta)

instance IsOption (Maybe JSONMeta) where
  defaultValue :: Maybe JSONMeta
  defaultValue = Just $ JSONMeta HM.empty
  parseValue :: String -> Maybe (Maybe JSONMeta)
  parseValue = Just . Just . JSONMeta . helper
    where
      splitPair :: Text -> (String, String)
      splitPair t = case split (== '=') t of
        [a, b] -> (unpack a, unpack b)
        _ -> error "json-meta: invalid input; ensure that '=' appears exactly once, separating the from the value"

      helper :: String -> HashMap String String
      helper s = HM.fromList $ map splitPair $ split (== ',') (pack s)
  optionName :: Tagged (Maybe JSONMeta) String
  optionName = Tagged "json-meta"
  optionHelp :: Tagged (Maybe JSONMeta) String
  optionHelp = Tagged "A comma-delimited set of key-value pairs separated by an equals sign to add to the JSON output"
