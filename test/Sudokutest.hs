{-# LANGUAGE OverloadedStrings #-}

module Main where

import Sudoku
import Test.HUnit
import Data.Maybe (isJust, isNothing)
import Data.List (nub, sort)
import Control.Monad (forM_)
import qualified Data.Text as T

createGrid :: [[Int]] -> [[Maybe Int]]
createGrid = map (map toMaybe)
  where
    toMaybe 0 = Nothing
    toMaybe n = Just n

isConsistent :: [[Maybe Int]] -> [[Maybe Int]] -> Bool
isConsistent original solution = all cellsMatch positions
  where
    positions = [(r, c) | r <- [0..8], c <- [0..8]]
    cellsMatch (r, c) = case original !! r !! c of
        Nothing -> True
        Just v  -> solution !! r !! c == Just v

testCheckSudoku :: Test
testCheckSudoku = TestLabel "Test checkSudoku function" $ TestList
    [ TestCase $ do
        assertBool "Valid complete grid" (checkSudoku validCompleteGrid)
        putStrLn " Completed: Valid complete grid"
    , TestCase $ do
        assertBool "Valid incomplete grid" (checkSudoku validIncompleteGrid)
        putStrLn " Completed: Valid incomplete grid"
    , TestCase $ do
        assertBool "Invalid grid (row conflict)" (not $ checkSudoku invalidGridRow)
        putStrLn " Completed: Invalid grid (row conflict)"
    , TestCase $ do
        assertBool "Invalid grid (column conflict)" (not $ checkSudoku invalidGridCol)
        putStrLn " Completed: Invalid grid (column conflict)"
    , TestCase $ do
        assertBool "Invalid grid (box conflict)" (not $ checkSudoku invalidGridBox)
        putStrLn " Completed: Invalid grid (box conflict)"
    , TestCase $ do
        assertBool "Empty grid is valid" (checkSudoku emptyGrid)
        putStrLn " Completed: Empty grid is valid"
    , TestCase $ do
        assertBool "Grid with numbers out of range is invalid" (not $ checkSudoku invalidGridOutOfRange)
        putStrLn " Completed: Grid with numbers out of range"
    ]

testSolveSudoku :: Test
testSolveSudoku = TestLabel "Test solveSudoku function" $ TestList
    [ TestCase $ do
        result <- solveSudoku validIncompleteGrid
        case result of
            Left errMsg -> assertFailure $ "Failed to solve valid puzzle: " ++ errMsg
            Right solution -> do
                assertBool "Solution is valid" (checkSudoku solution)
                assertBool "Solution is consistent with original puzzle" (isConsistent validIncompleteGrid solution)
                putStrLn " Completed: Solved valid incomplete grid"
    , TestCase $ do
        result <- solveSudoku invalidGridRow
        case result of
            Left _ -> do
                putStrLn " Completed: Correctly failed to solve invalid grid (row conflict)"
                return () 
            Right _ -> assertFailure "Solved an invalid puzzle (row conflict)"
    , TestCase $ do
        result <- solveSudoku invalidGridCol
        case result of
            Left _ -> do
                putStrLn " Completed: Correctly failed to solve invalid grid (column conflict)"
                return () 
            Right _ -> assertFailure "Solved an invalid puzzle (column conflict)"
    , TestCase $ do
        result <- solveSudoku invalidGridBox
        case result of
            Left _ -> do
                putStrLn " Completed: Correctly failed to solve invalid grid (box conflict)"
                return ()
            Right _ -> assertFailure "Solved an invalid puzzle (box conflict)"
    , TestCase $ do
        result <- solveSudoku emptyGrid
        case result of
            Left errMsg -> assertFailure $ "Failed to solve empty grid: " ++ errMsg
            Right solution -> do
                assertBool "Solution is valid for empty grid" (checkSudoku solution)
                putStrLn " Completed: Solved empty grid"
    , TestCase $ do
        result <- solveSudoku hardPuzzle
        case result of
            Left errMsg -> assertFailure $ "Failed to solve hard puzzle: " ++ errMsg
            Right solution -> do
                assertBool "Solution is valid for hard puzzle" (checkSudoku solution)
                assertBool "Solution is consistent with original hard puzzle" (isConsistent hardPuzzle solution)
                putStrLn " Completed: Solved hard puzzle"
    ]

testGenerateSudoku :: Test
testGenerateSudoku = TestLabel "Test generateSudoku function" $ TestCase $ do
    let difficulties = ["easy", "medium", "hard", "expert"]
    forM_ difficulties $ \difficulty -> do
        puzzle <- generateSudoku (T.pack difficulty)
        assertBool ("Generated puzzle is valid (" ++ difficulty ++ ")") (checkSudoku puzzle)
        result <- solveSudoku puzzle
        case result of
            Left errMsg -> assertFailure $ "Failed to solve generated puzzle (" ++ difficulty ++ "): " ++ errMsg
            Right solution -> do
                assertBool ("Solution is valid (" ++ difficulty ++ ")") (checkSudoku solution)
                assertBool ("Solution is consistent with puzzle (" ++ difficulty ++ ")") (isConsistent puzzle solution)
                putStrLn $ " Completed: Generated and solved puzzle (" ++ difficulty ++ ")"

validCompleteGrid :: [[Maybe Int]]
validCompleteGrid = createGrid
    [ [5,3,4,6,7,8,9,1,2]
    , [6,7,2,1,9,5,3,4,8]
    , [1,9,8,3,4,2,5,6,7]
    , [8,5,9,7,6,1,4,2,3]
    , [4,2,6,8,5,3,7,9,1]
    , [7,1,3,9,2,4,8,5,6]
    , [9,6,1,5,3,7,2,8,4]
    , [2,8,7,4,1,9,6,3,5]
    , [3,4,5,2,8,6,1,7,9]
    ]

validIncompleteGrid :: [[Maybe Int]]
validIncompleteGrid = createGrid
    [ [5,3,0,0,7,0,0,0,0]
    , [6,0,0,1,9,5,0,0,0]
    , [0,9,8,0,0,0,0,6,0]
    , [8,0,0,0,6,0,0,0,3]
    , [4,0,0,8,0,3,0,0,1]
    , [7,0,0,0,2,0,0,0,6]
    , [0,6,0,0,0,0,2,8,0]
    , [0,0,0,4,1,9,0,0,5]
    , [0,0,0,0,8,0,0,7,9]
    ]

invalidGridRow :: [[Maybe Int]]
invalidGridRow = createGrid
    [ [5,3,3,0,7,0,0,0,0] -- Conflict: two 3's in first row
    , [6,0,0,1,9,5,0,0,0]
    , [0,9,8,0,0,0,0,6,0]
    , [8,0,0,0,6,0,0,0,3]
    , [4,0,0,8,0,3,0,0,1]
    , [7,0,0,0,2,0,0,0,6]
    , [0,6,0,0,0,0,2,8,0]
    , [0,0,0,4,1,9,0,0,5]
    , [0,0,0,0,8,0,0,7,9]
    ]

invalidGridCol :: [[Maybe Int]]
invalidGridCol = createGrid
    [ [5,3,0,0,7,0,0,0,0]
    , [6,0,0,1,9,5,0,0,0]
    , [5,9,8,0,0,0,0,6,0] -- Conflict: two 5's in first column
    , [8,0,0,0,6,0,0,0,3]
    , [4,0,0,8,0,3,0,0,1]
    , [7,0,0,0,2,0,0,0,6]
    , [0,6,0,0,0,0,2,8,0]
    , [0,0,0,4,1,9,0,0,5]
    , [0,0,0,0,8,0,0,7,9]
    ]

invalidGridBox :: [[Maybe Int]]
invalidGridBox = createGrid
    [ [5,3,0,0,7,0,0,0,0]
    , [6,0,0,1,9,5,0,0,0]
    , [0,9,5,0,0,0,0,6,0] -- Conflict: two 5's in the top-left box
    , [8,0,0,0,6,0,0,0,3]
    , [4,0,0,8,0,3,0,0,1]
    , [7,0,0,0,2,0,0,0,6]
    , [0,6,0,0,0,0,2,8,0]
    , [0,0,0,4,1,9,0,0,5]
    , [0,0,0,0,8,0,0,7,9]
    ]

emptyGrid :: [[Maybe Int]]
emptyGrid = replicate 9 (replicate 9 Nothing)

invalidGridOutOfRange :: [[Maybe Int]]
invalidGridOutOfRange = createGrid
    [ [10,3,0,0,7,0,0,0,0] -- 10 is out of range
    , [6,0,0,1,9,5,0,0,0]
    , [0,9,8,0,0,0,0,6,0]
    , [8,0,0,0,6,0,0,0,3]
    , [4,0,0,8,0,3,0,0,1]
    , [7,0,0,0,2,0,0,0,6]
    , [0,6,0,0,0,0,2,8,0]
    , [0,0,0,4,1,9,0,0,5]
    , [0,0,0,0,8,0,0,7,9]
    ]

hardPuzzle :: [[Maybe Int]]
hardPuzzle = createGrid
    [ [0,0,0,0,0,0,0,1,2]
    , [0,0,0,0,3,5,0,0,0]
    , [0,0,0,0,0,0,0,0,0]
    , [0,1,0,0,0,0,0,0,0]
    , [0,0,0,0,8,0,0,0,0]
    , [0,0,0,0,0,0,0,4,0]
    , [0,0,0,0,0,0,7,0,0]
    , [0,0,0,1,2,0,0,0,0]
    , [3,4,0,0,0,0,0,0,0]
    ]

main :: IO ()
main = do
  counts <- runTestTT $ TestList [testCheckSudoku, testSolveSudoku, testGenerateSudoku]
  putStrLn $ "Ran " ++ show (cases counts) ++ " tests."
  if errors counts + failures counts == 0
    then putStrLn "All tests passed."
    else putStrLn $ "Tests failed: " ++ show (errors counts + failures counts)
