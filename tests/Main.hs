module Main where

import Test.Tasty (defaultMainWithIngredients, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Runners.JSONReporter (reporters)

main :: IO ()
-- A simple test tree using Tasty
main = defaultMainWithIngredients [reporters] tests
  where
    tests =
      testGroup
        "Tests"
        [ testGroup
            "Group 1"
            [ testGroup
                "Group 1.1"
                [ testGroup
                    "Group 1.1.1"
                    [ testCase "Test 1" $ assertEqual "Test 1" 1 1,
                      testCase "Test 2" $ assertEqual "Expected Failure" 2 3
                    ],
                  testCase "Test 3" $ assertEqual "Test 3" 3 3
                ],
              testCase "Test 4" $ assertEqual "Test 4" 4 4
            ],
          testGroup
            "Group 2"
            [ testCase "Test 5" $ assertEqual "Test 5" 5 5
            ]
        ]
