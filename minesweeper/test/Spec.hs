{-# LANGUAGE NamedFieldPuns #-}

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Control.Monad (forM_)
import qualified Data.Vector as V
import qualified Data.Set as S

import Cell
import Grid.Core (Grid)
import Game (parseInput, validateParameters, processMove)
import Grid.Operation (isOutOfBounds, getNeighborCoords, revealCell, flagCell)
import Grid.Generation (generateEmptyGrid, generateRandomGrid)

-- Unit Tests
unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ parseInputTests
  , validateParametersTests
  , isOutOfBoundsTests
  , flagCellTests
  , revealCellTests
  , gridGenerationTests
  ]

-- Parse Input Tests
parseInputTests :: TestTree
parseInputTests = testGroup "Parse Input Tests"
  [ testCase "Valid Input without Flag" $
      parseInput "1,2" @?= Just (False, 1, 2)
  , testCase "Valid Input with Flag" $
      parseInput "F 0,3" @?= Just (True, 0, 3)
  , testCase "Valid Input with Extra Spaces" $
      parseInput "  F  2, 4  " @?= Just (True, 2, 4)
  , testCase "Invalid Format - Missing Comma" $
      parseInput "1 2" @?= Nothing
  , testCase "Invalid Format - Non-integer" $
      parseInput "a,b" @?= Nothing
  , testCase "Invalid Format - Empty Input" $
      parseInput "" @?= Nothing
  , testCase "Invalid Format - Negative Indices" $
      parseInput "F -1,-2" @?= Just (True, -1, -2)
  ]

-- Validate Parameters Tests
validateParametersTests :: TestTree
validateParametersTests = testGroup "Validate Parameters Tests"
  [ testCase "Valid Parameters" $
      validateParameters 5 5 10 @?= Right ()
  , testCase "Zero Dimensions" $
      validateParameters 0 5 5 @?= Left "Grid dimensions must be greater than 0."
  , testCase "Negative Dimensions" $
      validateParameters (-1) 5 5 @?= Left "Grid dimensions must be greater than 0."
  , testCase "Too Many Mines" $
      validateParameters 5 5 26 @?= Left "Number of mines must be less than the total number of cells."
  , testCase "Negative Mines" $
      validateParameters 5 5 (-1) @?= Left "Number of mines must be non-negative."
  ]

-- Is Out of Bounds Tests
isOutOfBoundsTests :: TestTree
isOutOfBoundsTests = testGroup "Is Out of Bounds Tests"
  [ testCase "Within Bounds" $
      isOutOfBounds 2 2 (generateEmptyGrid 5 5) @?= False
  , testCase "Row Out of Bounds (Negative)" $
      isOutOfBounds (-1) 0 (generateEmptyGrid 5 5) @?= True
  , testCase "Row Out of Bounds (Exceeds)" $
      isOutOfBounds 5 0 (generateEmptyGrid 5 5) @?= True
  , testCase "Column Out of Bounds (Negative)" $
      isOutOfBounds 0 (-1) (generateEmptyGrid 5 5) @?= True
  , testCase "Column Out of Bounds (Exceeds)" $
      isOutOfBounds 0 5 (generateEmptyGrid 5 5) @?= True
  ]

-- Flag Cell Tests
flagCellTests :: TestTree
flagCellTests = testGroup "Flag Cell Tests"
  [ testCase "Flagging a Cell" $ do
      let grid = generateEmptyGrid 5 5
          flaggedGrid = flagCell 2 2 grid
          cell = (grid V.! 2) V.! 2
          flaggedCell = (flaggedGrid V.! 2) V.! 2
      isFlagged cell @?= False
      isFlagged flaggedCell @?= True
  , testCase "Unflagging a Cell" $ do
      let grid = flagCell 2 2 (generateEmptyGrid 5 5)
          unflaggedGrid = flagCell 2 2 grid
          cell = (unflaggedGrid V.! 2) V.! 2
      isFlagged cell @?= False
  , testCase "Flagging a Revealed Cell" $ do
      let grid = generateEmptyGrid 5 5
          revealedGrid = revealCell 2 2 grid
          result = processMove True 2 2 revealedGrid  -- Use processMove instead of flagCell
      result @?= Left "Cannot flag a revealed cell."  -- Validate the error message
  ]

-- Reveal Cell Tests
revealCellTests :: TestTree
revealCellTests = testGroup "Reveal Cell Tests"
  [ testCase "Revealing a Hidden Cell" $ do
      let grid = generateEmptyGrid 5 5
          revealedGrid = revealCell 2 2 grid
          cell = (revealedGrid V.! 2) V.! 2
      isRevealed cell @?= True
  ]

-- Grid Generation Tests
gridGenerationTests :: TestTree
gridGenerationTests = testGroup "Grid Generation Tests"
  [ testCase "Generate Empty Grid" $ do
      let grid = generateEmptyGrid 5 5
      V.length grid @?= 5
      V.length (grid V.! 0) @?= 5
  , testCase "Generate Random Grid with Correct Mine Count" $ do
      let rows = 5
          cols = 5
          mines = 10
      grid <- generateRandomGrid rows cols mines (0, 0)
      let mineCount = V.sum $ V.map (V.length . V.filter isMine) grid
      mineCount @?= mines
  ]

-- Property Tests
genCell :: Gen Cell
genCell = do
  isMine <- Gen.bool
  isRevealed <- Gen.bool
  isFlagged <- Gen.bool
  adjacentMines <- Gen.int (Range.constant 0 8)
  return Cell { isMine, isRevealed, isFlagged, adjacentMines }

genGrid :: Gen Grid
genGrid = do
  rows <- Gen.int (Range.constant 1 10)
  cols <- Gen.int (Range.constant 1 10)
  V.replicateM rows (V.replicateM cols genCell)

prop_flagCell_toggle :: Property
prop_flagCell_toggle = property $ do
  grid <- forAll genGrid
  row <- forAll $ Gen.int (Range.constant 0 (V.length grid - 1))
  col <- forAll $ Gen.int (Range.constant 0 (V.length (grid V.! 0) - 1))
  let flaggedGrid = flagCell row col grid
      unflaggedGrid = flagCell row col flaggedGrid
      cellOriginal = (grid V.! row) V.! col
      cellFlagged = (flaggedGrid V.! row) V.! col
      cellUnflagged = (unflaggedGrid V.! row) V.! col
  Hedgehog.assert $ isFlagged cellOriginal /= isFlagged cellFlagged
  Hedgehog.assert $ isFlagged cellOriginal == isFlagged cellUnflagged

propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ testProperty "flagCell toggles flag state" prop_flagCell_toggle
  ]

main :: IO ()
main = defaultMain $ testGroup "Minesweeper Tests" [unitTests, propertyTests]