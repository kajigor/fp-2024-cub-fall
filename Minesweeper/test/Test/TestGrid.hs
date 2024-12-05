module Test.TestGrid where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog as H
import Control.Monad.IO.Class (liftIO)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Grid
import Data.List (concat)

-- Extract Cell from (VisibleState, Cell)
extractCell :: (VisibleState, Cell) -> Cell
extractCell (_, cell) = cell

-- Property: Mine count matches input parameter
prop_correctMineCount :: Property
prop_correctMineCount = property $ do
  rows <- forAll $ Gen.int (Range.linear 5 20)
  cols <- forAll $ Gen.int (Range.linear 5 20)
  mines <- forAll $ Gen.int (Range.linear 1 (rows * cols `div` 2))
  grid <- liftIO $ initializeGrid rows cols mines Nothing
  let mineCount = length $ concatMap (filter ((== Mine) . extractCell)) grid
  H.assert (mineCount == mines)

-- Property: Numbers on the grid correctly represent adjacent mines
prop_correctNumberCalculation :: Property
prop_correctNumberCalculation = property $ do
  rows <- forAll $ Gen.int (Range.linear 5 10)
  cols <- forAll $ Gen.int (Range.linear 5 10)
  mines <- forAll $ Gen.int (Range.linear 1 (rows * cols `div` 2))
  grid <- liftIO $ initializeGrid rows cols mines Nothing
  let isMine (r, c) = case safeAccess grid r c of
                        Just (_, Mine) -> True
                        _              -> False
      countAdjacentMines r c = length $ filter isMine [(r-1, c-1), (r-1, c), (r-1, c+1),
                                                       (r,   c-1),           (r,   c+1),
                                                       (r+1, c-1), (r+1, c), (r+1, c+1)]
      isValidNumber (r, c) = case safeAccess grid r c of
            Just (_, Empty n) -> n == countAdjacentMines r c
            _                 -> True
  let allValid = all (\(r, c) -> isValidNumber (r, c)) [(r, c) | r <- [0..rows-1], c <- [0..cols-1]]
  H.assert allValid

-- Fixed tests
testMinePlacement :: IO ()
testMinePlacement = do
    let rows = 5
        cols = 5
        mines = 5
        minePositions = [(0, 0), (1, 1), (2, 2), (3, 3), (4, 4)]
    grid <- initializeGrid rows cols mines (Just minePositions)
    let mineCount = length $ concatMap (filter ((== Mine) . extractCell)) grid
    assertEqual "Number of mines should match" mines mineCount

testNumberCalculation :: IO ()
testNumberCalculation = do
    let rows = 3
        cols = 3
        mines = 1
        minePositions = [(1, 1)]
    grid <- initializeGrid rows cols mines (Just minePositions)
    let isMine (r, c) = case safeAccess grid r c of
                          Just (_, Mine) -> True
                          _              -> False
        countAdjacentMines r c = length $ filter isMine [(r-1, c-1), (r-1, c), (r-1, c+1),
                                                         (r,   c-1),           (r,   c+1),
                                                         (r+1, c-1), (r+1, c), (r+1, c+1)]
        isValidNumber (r, c) = case safeAccess grid r c of
            Just (_, Empty n) -> n == countAdjacentMines r c
            _                 -> True
    let allValid = and [isValidNumber (r, c) | r <- [0..rows-1], c <- [0..cols-1]]
    assertBool "Numbers on the grid should be accurate" allValid

-- Helper: Safe access to grid cells
safeAccess :: [[a]] -> Int -> Int -> Maybe a
safeAccess grid r c
  | r < 0 || r >= length grid = Nothing
  | c < 0 || c >= length (head grid) = Nothing
  | otherwise = Just (grid !! r !! c)

-- Group Tests
tests :: TestTree
tests = testGroup "Grid Tests"
    [ testProperty "Correct mine count" prop_correctMineCount
    , testProperty "Accurate number calculation" prop_correctNumberCalculation
    , testCase "Fixed mine placement" (liftIO testMinePlacement)
    , testCase "Fixed number calculation" (liftIO testNumberCalculation)
    ]
