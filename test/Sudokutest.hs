{-# LANGUAGE OverloadedStrings #-}

module Main where

import Sudoku
import qualified Test.HUnit as HUnit
import Control.Monad (forM_)
import qualified Data.Text as T

-- Hedgehog imports
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Control.Monad.IO.Class (liftIO)

-- Helper to create a grid from a list of lists
createGrid :: [[Int]] -> [[Maybe Int]]
createGrid = map (map toMaybe)
  where
    toMaybe 0 = Nothing
    toMaybe n = Just n

-- Check if the solution is consistent with the original puzzle
isConsistent :: [[Maybe Int]] -> [[Maybe Int]] -> Bool
isConsistent original solution = all cellsMatch positions
  where
    positions = [(r, c) | r <- [0..8], c <- [0..8]]
    cellsMatch (r, c) = case original !! r !! c of
        Nothing -> True
        Just v  -> solution !! r !! c == Just v

-- Unit Tests (HUnit)
testCheckSudoku :: HUnit.Test
testCheckSudoku = HUnit.TestLabel "Test checkSudoku function" $ HUnit.TestList
    [ HUnit.TestCase $ do
        HUnit.assertBool "Valid complete grid" (checkSudoku validCompleteGrid)
        putStrLn " Completed: Valid complete grid"
    , HUnit.TestCase $ do
        HUnit.assertBool "Valid incomplete grid" (checkSudoku validIncompleteGrid)
        putStrLn " Completed: Valid incomplete grid"
    , HUnit.TestCase $ do
        HUnit.assertBool "Invalid grid (row conflict)" (not $ checkSudoku invalidGridRow)
        putStrLn " Completed: Invalid grid (row conflict)"
    , HUnit.TestCase $ do
        HUnit.assertBool "Invalid grid (column conflict)" (not $ checkSudoku invalidGridCol)
        putStrLn " Completed: Invalid grid (column conflict)"
    , HUnit.TestCase $ do
        HUnit.assertBool "Invalid grid (box conflict)" (not $ checkSudoku invalidGridBox)
        putStrLn " Completed: Invalid grid (box conflict)"
    , HUnit.TestCase $ do
        HUnit.assertBool "Empty grid is valid" (checkSudoku emptyGrid)
        putStrLn " Completed: Empty grid is valid"
    , HUnit.TestCase $ do
        HUnit.assertBool "Grid with numbers out of range is invalid" (not $ checkSudoku invalidGridOutOfRange)
        putStrLn " Completed: Grid with numbers out of range"
    ]

testSolveSudoku :: HUnit.Test
testSolveSudoku = HUnit.TestLabel "Test solveSudoku function" $ HUnit.TestList
    [ HUnit.TestCase $ do
        result <- solveSudoku validIncompleteGrid
        case result of
            Left errMsg -> HUnit.assertFailure $ "Failed to solve valid puzzle: " ++ errMsg
            Right solution -> do
                HUnit.assertBool "Solution is valid" (checkSudoku solution)
                HUnit.assertBool "Solution is consistent with original puzzle" (isConsistent validIncompleteGrid solution)
                putStrLn " Completed: Solved valid incomplete grid"
    , HUnit.TestCase $ do
        result <- solveSudoku invalidGridRow
        case result of
            Left _ -> putStrLn " Completed: Correctly failed to solve invalid grid (row conflict)"
            Right _ -> HUnit.assertFailure "Solved an invalid puzzle (row conflict)"
    , HUnit.TestCase $ do
        result <- solveSudoku invalidGridCol
        case result of
            Left _ -> putStrLn " Completed: Correctly failed to solve invalid grid (column conflict)"
            Right _ -> HUnit.assertFailure "Solved an invalid puzzle (column conflict)"
    , HUnit.TestCase $ do
        result <- solveSudoku invalidGridBox
        case result of
            Left _ -> putStrLn " Completed: Correctly failed to solve invalid grid (box conflict)"
            Right _ -> HUnit.assertFailure "Solved an invalid puzzle (box conflict)"
    , HUnit.TestCase $ do
        result <- solveSudoku emptyGrid
        case result of
            Left errMsg -> HUnit.assertFailure $ "Failed to solve empty grid: " ++ errMsg
            Right solution -> do
                HUnit.assertBool "Solution is valid for empty grid" (checkSudoku solution)
                putStrLn " Completed: Solved empty grid"
    , HUnit.TestCase $ do
        result <- solveSudoku hardPuzzle
        case result of
            Left errMsg -> HUnit.assertFailure $ "Failed to solve hard puzzle: " ++ errMsg
            Right solution -> do
                HUnit.assertBool "Solution is valid for hard puzzle" (checkSudoku solution)
                HUnit.assertBool "Solution is consistent with original hard puzzle" (isConsistent hardPuzzle solution)
                putStrLn " Completed: Solved hard puzzle"
    ]

testGenerateSudoku :: HUnit.Test
testGenerateSudoku = HUnit.TestLabel "Test generateSudoku function" $ HUnit.TestCase $ do
    let difficulties = ["easy", "medium", "hard", "expert"]
    forM_ difficulties $ \difficulty -> do
        puzzle <- generateSudoku (T.pack difficulty)
        HUnit.assertBool ("Generated puzzle is valid (" ++ difficulty ++ ")") (checkSudoku puzzle)
        result <- solveSudoku puzzle
        case result of
            Left errMsg -> HUnit.assertFailure $ "Failed to solve generated puzzle (" ++ difficulty ++ "): " ++ errMsg
            Right solution -> do
                HUnit.assertBool ("Solution is valid (" ++ difficulty ++ ")") (checkSudoku solution)
                HUnit.assertBool ("Solution is consistent with puzzle (" ++ difficulty ++ ")") (isConsistent puzzle solution)
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
    [ [5,3,3,0,7,0,0,0,0]
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
    , [5,9,8,0,0,0,0,6,0]
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
    , [0,9,5,0,0,0,0,6,0]
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
    [ [10,3,0,0,7,0,0,0,0]
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

-- Property-based tests with Hedgehog
cellGen :: Gen (Maybe Int)
cellGen = Gen.frequency
  [ (1, pure Nothing)
  , (9, Just <$> Gen.int (Range.constant 1 9))
  ]

gridGen :: Gen [[Maybe Int]]
gridGen = Gen.list (Range.singleton 9) (Gen.list (Range.singleton 9) cellGen)

prop_randomPuzzle :: Property
prop_randomPuzzle = property $ do
  grid <- forAll gridGen
  result <- liftIO $ solveSudoku grid
  case result of
    Right solution -> checkSudoku solution === True
    Left _ -> success

main :: IO ()
main = do
  counts <- HUnit.runTestTT $ HUnit.TestList [testCheckSudoku, testSolveSudoku, testGenerateSudoku]
  putStrLn $ "Ran " ++ show (HUnit.cases counts) ++ " tests."
  if HUnit.errors counts + HUnit.failures counts == 0
    then putStrLn "All unit tests passed."
    else putStrLn $ "Tests failed: " ++ show (HUnit.errors counts + HUnit.failures counts)

  propRes <- check prop_randomPuzzle
  if not propRes
    then putStrLn "Property test failed"
    else putStrLn "Property tests passed"
