module Main where

import Test.HUnit
import Data.List (nub)
import qualified Data.Vector as V

import Board
import Solver (solve)
import SampleBoards (board1, board1Solved, invalidBoard, boardHard)

isValidSolution :: Board -> Bool
isValidSolution board = all isValidRow board && all isValidColumn [0..8] && all isValidSubgrid [0..2]
  where
    isValidRow row = let values = V.toList row in length (filter (/= 0) values) == length (nub values)
    isValidColumn colIndex = let values = map (V.! colIndex) (V.toList board) in length (filter (/= 0) values) == length (nub values)
    isValidSubgrid gridIndex = let subgrid = [ board V.! (row + 3 * (gridIndex `div` 3)) V.! (col + 3 * (gridIndex `mod` 3)) | row <- [0..2], col <- [0..2]]
      in length (filter (/= 0) subgrid) == length (nub subgrid)

testSolutionIsValid :: Test
testSolutionIsValid = TestCase $ do
  let solved = solve board1
  case solved of
    Left err -> assertFailure "No solution found"
    Right res -> isValidSolution res @?= True

findZeroes :: Board -> [(Int, Int)]
findZeroes board = filter (\(r, c) -> (board V.! r) V.! c == 0) [(r, c) | r <- [0..8], c <- [0..8]]

nonZeroesMatch :: Board -> Board -> Board -> Bool
nonZeroesMatch init solved correct = all (\(r, c) -> solved V.! r V.! c == correct V.! r V.! c) (findZeroes init)

testInitialNumbersAreSame :: Test
testInitialNumbersAreSame = TestCase $ do
    let solved = solve board1
    case solved of
        Left err -> assertFailure "No solution found"
        Right res -> nonZeroesMatch board1 res board1Solved @?= True

testInvalidSudokuIsNotSolved :: Test
testInvalidSudokuIsNotSolved = TestCase $ do
    let solved = solve invalidBoard
    case solved of
        Left err -> True @?= True
        Right res -> assertFailure "Invalid sudoku is solved"


testHardBoardSolved :: Test
testHardBoardSolved = TestCase $ do
    let solved = solve boardHard
    case solved of
        Left err -> assertFailure "No solution found"
        Right res -> isValidSolution res @?= True


tests :: Test
tests = TestList [ testSolutionIsValid, testInitialNumbersAreSame, testInvalidSudokuIsNotSolved, testHardBoardSolved ]

main :: IO Counts
main = runTestTT tests
