{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson (ToJSON (toJSON), eitherDecodeFileStrict')
import System.Environment (getArgs)
import Test.Tasty.Types.JSONTestSuiteResult (JSONTestSuiteResult (..))

defaultMain :: String -> IO ()
defaultMain jsonPath =
  (eitherDecodeFileStrict' jsonPath) >>= \case
    Left err -> putStrLn err
    Right (json :: JSONTestSuiteResult) -> print $ toJSON json

main :: IO ()
main =
  getArgs >>= \case
    [jsonPath] -> defaultMain jsonPath
    _ -> putStrLn "Usage: tasty-json-markdown <json-path>"
