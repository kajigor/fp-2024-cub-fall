{-# LANGUAGE ScopedTypeVariables #-}

module Test.TestErrorHandling where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import ErrorHandling
import Grid



unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Invalid move: negative indices" $
      validateMove [[Hidden, Hidden], [Hidden, Hidden]] (-1, 0) @?= Left (InvalidMove "Move is out of bounds (negative indices).")

  , testCase "Invalid move: indices exceed grid bounds" $
      validateMove [[Hidden, Hidden], [Hidden, Hidden]] (2, 0) @?= Left (InvalidMove "Move is out of bounds (indices exceed grid dimensions).")

  , testCase "Valid move" $
      validateMove [[Hidden, Hidden], [Hidden, Hidden]] (0, 1) @?= Right ()

  , testCase "Invalid grid dimensions" $
      validateGridDimensions (-1) 5 @?= Left (InitializationError "Grid dimensions must be positive integers.")

  , testCase "Invalid mine count: negative" $
      validateMineCount 5 5 (-1) @?= Left (InitializationError "Number of mines cannot be negative.")

  , testCase "Invalid mine count: exceeds grid capacity" $
      validateMineCount 5 5 30 @?= Left (InitializationError "Number of mines exceeds grid capacity.")
  ]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ testProperty "Move validation always fails for negative indices" $ property $ do
      grid <- forAll $ generateGrid 5 5
      row <- forAll $ Gen.int (Range.linear (-10) (-1))
      col <- forAll $ Gen.int (Range.linear (-10) (-1))
      validateMove grid (row, col) === Left (InvalidMove "Move is out of bounds (negative indices).")

  , testProperty "Valid grid dimensions always pass" $ property $ do
      rows <- forAll $ Gen.int (Range.linear 1 100)
      cols <- forAll $ Gen.int (Range.linear 1 100)
      validateGridDimensions rows cols === Right ()
  ]

-- Helper: Generate a random grid for testing
generateGrid :: Int -> Int -> Gen Grid
generateGrid rows cols = do
  return (replicate rows (replicate cols Hidden))

tests :: TestTree
tests = testGroup "Error Handling Tests" [unitTests, propertyTests]
