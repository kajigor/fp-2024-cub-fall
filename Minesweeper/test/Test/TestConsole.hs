module Test.TestConsole where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import ConsoleUI
import Grid
import GameLogic
import Control.Exception (evaluate)
import System.IO.Silently (capture_)

-- Unit tests for parsing moves
testParseMove :: TestTree
testParseMove = testGroup "parseMove Tests"
  [ testCase "Valid move (reveal)" $
      parseMove "1 2 r" @?= Just (1, 2, 'r')
  , testCase "Valid move (flag)" $
      parseMove "3 4 f" @?= Just (3, 4, 'f')
  , testCase "Invalid action" $
      parseMove "3 4 x" @?= Nothing
  , testCase "Invalid format" $
      parseMove "invalid" @?= Nothing
  ]

-- Unit test for grid printing output
testPrintGrid :: TestTree
testPrintGrid = testGroup "printGrid Tests"
  [ testCase "Hidden grid print contains '■'" $ do
      output <- capture_ (printGrid (replicate 5 (replicate 5 Hidden)))
      assertBool "Grid should contain '■'" ("■" `elem` words output)
  ]

-- Property-based tests for parsing moves
prop_parseMove :: Property
prop_parseMove = property $ do
  r <- forAll $ Gen.int (Range.linear 0 20)
  c <- forAll $ Gen.int (Range.linear 0 20)
  action <- forAll $ Gen.element ['r', 'f']
  parseMove (show r ++ " " ++ show c ++ " " ++ [action]) === Just (r, c, action)

-- Property-based tests for random game initialization
prop_initializeGrid :: Property
prop_initializeGrid = property $ do
  rows <- forAll $ Gen.int (Range.linear 5 20)
  cols <- forAll $ Gen.int (Range.linear 5 20)
  mines <- forAll $ Gen.int (Range.linear 1 (rows * cols `div` 2))
  grid <- evalIO $ initializeGrid rows cols mines Nothing
  -- Verify grid has correct dimensions
  length grid === rows
  length (head grid) === cols

-- Property-based test for flag-reveal cycle
prop_flagRevealCycle :: Property
prop_flagRevealCycle = property $ do
  let emptyGrid = replicate 5 (replicate 5 Hidden)
  r <- forAll $ Gen.int (Range.linear 0 4)
  c <- forAll $ Gen.int (Range.linear 0 4)
  let grid = flagCell emptyGrid (r, c)
  case getCell grid (r, c) of
    Just cell -> cell === Flagged
    Nothing -> failure -- This shouldn't happen for valid grid coordinates


tests :: TestTree
tests = testGroup "Grid Tests"
    [ testParseMove
    , testPrintGrid
    , testPropertyBased
    ]

testPropertyBased :: TestTree
testPropertyBased = testGroup "Property-Based Tests"
  [ testProperty "Valid parseMove" prop_parseMove
  , testProperty "Initialize Grid Correctly" prop_initializeGrid
  , testProperty "Flag Reveal Cycle Test" prop_flagRevealCycle
  ]
