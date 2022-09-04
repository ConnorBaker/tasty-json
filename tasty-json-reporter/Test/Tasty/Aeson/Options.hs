module Test.Tasty.Aeson.Options (tastyJSONOptions) where

import Data.Aeson (Options (..), defaultOptions)

tastyJSONOptions :: Options
tastyJSONOptions = defaultOptions {allNullaryToStringTag = False, omitNothingFields = True, tagSingleConstructors = True}
