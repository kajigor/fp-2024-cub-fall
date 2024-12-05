module Test.TestErrorHandling where

import Test.Tasty
import Test.Tasty.HUnit
import Grid
import ErrorHandling
-- Mock Grid for testing
mockGrid :: Grid
mockGrid = 
    [ [(Hidden, Empty 0), (Hidden, Empty 1), (Hidden, Mine)]
    , [(Hidden, Empty 2), (Revealed, Empty 0), (Hidden, Empty 1)]
    , [(Hidden, Empty 0), (Hidden, Empty 1), (Flagged, Mine)]
    ]

-- Test suite for error handling
tests :: TestTree
tests = testGroup "Error Handling Tests"
  [ testGroup "validateMove Tests"
      [ testCase "Move out of bounds (negative indices)" $
          validateMove mockGrid (-1, 0) @?= Left (InvalidMove "Move is out of bounds (negative indices).")

      , testCase "Move out of bounds (exceeds grid size)" $
          validateMove mockGrid (3, 0) @?= Left (InvalidMove "Move is out of bounds (exceeds grid size).")

      , testCase "Move on already revealed cell" $
          validateMove mockGrid (1, 1) @?= Left (InvalidMove "Move is invalid (cell already revealed or flagged).")

      , testCase "Move on flagged cell" $
          validateMove mockGrid (2, 2) @?= Left (InvalidMove "Move is invalid (cell already revealed or flagged).")

      , testCase "Valid move on hidden cell" $
          validateMove mockGrid (0, 0) @?= Right ()
      ]

  , testGroup "validateInput Tests"
      [ testCase "Valid input (reveal action)" $
          validateInput "0 1 r" @?= Right (0, 1, 'r')

      , testCase "Valid input (flag action)" $
          validateInput "2 2 f" @?= Right (2, 2, 'f')

      , testCase "Invalid input (non-integer row/col)" $
          validateInput "a 1 r" @?= Left (InvalidInput "Row and column must be integers.")

      , testCase "Invalid input (unsupported action)" $
          validateInput "1 1 x" @?= Left (InvalidInput "Action must be 'r' (reveal) or 'f' (flag).")

      , testCase "Invalid input format (too few arguments)" $
          validateInput "1 r" @?= Left (InvalidInput "Input must be in the format: row col action.")

      , testCase "Invalid input format (too many arguments)" $
          validateInput "1 1 r extra" @?= Left (InvalidInput "Input must be in the format: row col action.")
      ]

  , testGroup "validateGridInitialization Tests"
      [ testCase "Invalid dimensions (negative rows)" $
          validateGridInitialization (-1) 5 5 @?= Left (InitializationError "Grid dimensions must be positive integers.")

      , testCase "Invalid dimensions (zero columns)" $
          validateGridInitialization 5 0 5 @?= Left (InitializationError "Grid dimensions must be positive integers.")

      , testCase "Invalid mine count (negative mines)" $
          validateGridInitialization 5 5 (-1) @?= Left (InitializationError "Number of mines cannot be negative.")

      , testCase "Invalid mine count (too many mines)" $
          validateGridInitialization 5 5 30 @?= Left (InitializationError "Number of mines exceeds or equals the total number of cells.")

      , testCase "Valid grid initialization" $
          validateGridInitialization 5 5 10 @?= Right ()
      ]
  ]
