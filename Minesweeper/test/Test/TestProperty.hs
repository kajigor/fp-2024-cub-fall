module Test.TestProperty (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

tests :: TestTree
tests = testGroup "Property Tests"
    [ QC.testProperty "Example Property" $ \x -> x == (x :: Int)
    ]
