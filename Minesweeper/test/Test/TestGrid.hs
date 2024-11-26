module Test.TestGrid where

import Test.Tasty
import Test.Tasty.HUnit
import Grid
import Data.List (concat)

testMinePlacement :: TestTree
testMinePlacement = testCase "Correct mine placement" $ do
    let rows = 5
        cols = 5
        mines = 5
        minePositions = [(0,0), (1,1), (2,2), (3,3), (4,4)]
    grid <- initializeGrid rows cols mines (Just minePositions)
    let mineCount = length $ concatMap (filter (== Mine)) grid
    assertEqual "Number of mines should match" mines mineCount

testNumberCalculation :: TestTree
testNumberCalculation = testCase "Accurate calculation of numbers" $ do
    let rows = 3
        cols = 3
        mines = 1
        minePositions = [(1,1)]
    grid <- initializeGrid rows cols mines (Just minePositions)
    let isMine (r, c) = case safeAccess grid r c of
                          Just Mine -> True
                          _         -> False
        countAdjacentMines r c = length $ filter isMine [(r-1, c-1), (r-1, c), (r-1, c+1),
                                                         (r,   c-1),           (r,   c+1),
                                                         (r+1, c-1), (r+1, c), (r+1, c+1)]
    let isValidNumber (r, c) = case safeAccess grid r c of
          Just (Empty n) -> n == countAdjacentMines r c
          _              -> True
    let allValid = and [isValidNumber (r, c) | r <- [0..rows-1], c <- [0..cols-1]]
    assertBool "Numbers on the grid should be accurate" allValid

safeAccess :: [[a]] -> Int -> Int -> Maybe a
safeAccess grid r c
  | r < 0 || r >= length grid = Nothing
  | c < 0 || c >= length (head grid) = Nothing
  | otherwise = Just (grid !! r !! c)

tests :: TestTree
tests = testGroup "Grid Tests"
    [ testMinePlacement
    , testNumberCalculation
    ]
