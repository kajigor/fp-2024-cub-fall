module Test.TestGameLogic (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "GameLogic Tests"
    [ testCase "Example Test" $ assertEqual "Example" 1 1
    ]
