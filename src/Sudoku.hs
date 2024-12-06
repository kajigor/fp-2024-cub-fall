{-# LANGUAGE OverloadedStrings #-}

module Sudoku
    ( generateSudoku
    , encodeGrid
    , parseGrid
    , checkSudoku
    , solveSudoku
    , toStrict
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.Random (randomRIO)
import Data.Maybe (isJust, catMaybes, isNothing)
import Data.List (nub, transpose, sortOn, (\\))
import Text.Read (readMaybe)
import Data.Char (isDigit)
import System.Random.Shuffle (shuffleM)

generateSudoku :: Text -> IO [[Maybe Int]]
generateSudoku difficulty = do
    let emptyGrid = replicate 9 (replicate 9 Nothing)
    Right solvedGrid <- solveSudoku emptyGrid
    emptyCells <- case difficulty of
        "easy"   -> randomRIO (30, 40)
        "medium" -> randomRIO (41, 50)
        "hard"   -> randomRIO (51, 60)
        "expert" -> randomRIO (61, 68)
        _        -> randomRIO (30, 40)
    removeCells solvedGrid emptyCells

removeCells :: [[Maybe Int]] -> Int -> IO [[Maybe Int]]
removeCells grid n
    | n <= 0    = return grid
    | otherwise = do
        let nonEmptyCells = [(r, c) | r <- [0..8], c <- [0..8], isJust (grid !! r !! c)]
        if null nonEmptyCells
            then return grid  -- No more cells to remove
            else do
                idx <- randomRIO (0, length nonEmptyCells - 1)
                let (r, c) = nonEmptyCells !! idx
                let updatedGrid = updateGrid grid r c Nothing
                removeCells updatedGrid (n - 1)

updateGrid :: [[Maybe Int]] -> Int -> Int -> Maybe Int -> [[Maybe Int]]
updateGrid grid r c val =
    [if row == r then updateRow rowVals else rowVals | (row, rowVals) <- zip [0..] grid]
  where
    updateRow row = [if col == c then val else cell | (col, cell) <- zip [0..] row]

encodeGrid :: [[Maybe Int]] -> TL.Text
encodeGrid grid =
    TL.intercalate "&" [TL.pack (cellName r c ++ "=" ++ showValue (grid !! (r - 1) !! (c - 1))) | r <- [1..9], c <- [1..9]]
  where
    cellName r c = "cell-" ++ show r ++ "-" ++ show c
    showValue Nothing  = ""
    showValue (Just n) = show n

parseGrid :: [(Text, Text)] -> Either TL.Text [[Maybe Int]]
parseGrid params =
    traverse (traverse parseCell) [[lookup (cellName r c) params | c <- [1..9]] | r <- [1..9]]
  where
    cellName r c = T.pack $ "cell-" ++ show r ++ "-" ++ show c

    parseCell :: Maybe Text -> Either TL.Text (Maybe Int)
    parseCell (Just n)
        | T.null n  = Right Nothing
        | T.all isDigit n = case readMaybe (T.unpack n) of
            Just 0    -> Left "Error: Grid cannot contain a 0."
            Just value -> if value >= 1 && value <= 9
                            then Right (Just value)
                            else Left $ "Invalid number: " <> TL.pack (show value)
            Nothing    -> Left $ "Unexpected error parsing number: " <> TL.pack (T.unpack n)
        | otherwise = Left $ "Invalid input: " <> TL.pack (T.unpack n)
    parseCell Nothing = Right Nothing

-- Check if the Sudoku grid is valid
checkSudoku :: [[Maybe Int]] -> Bool
checkSudoku grid = checkSudokuRows grid && checkSudokuCols grid && checkSudokuBoxes grid && isValidState grid

checkSudokuRows, checkSudokuCols, checkSudokuBoxes :: [[Maybe Int]] -> Bool
checkSudokuRows = all allDistinctExceptNothing
checkSudokuCols = all allDistinctExceptNothing . transpose
checkSudokuBoxes = all (allDistinctExceptNothing . concat) . boxes

allDistinctExceptNothing :: (Eq a) => [Maybe a] -> Bool
allDistinctExceptNothing xs = let withoutNothing = catMaybes xs in nub withoutNothing == withoutNothing

boxes :: [[a]] -> [[[a]]]
boxes grid =
    [ [ [grid !! r !! c | c <- [cStart..cStart + 2]]
        | r <- [rStart..rStart + 2]
      ]
      | rStart <- [0, 3, 6], cStart <- [0, 3, 6]
    ]

-- Solve Sudoku puzzle using backtracking with MRV heuristic
solveSudoku :: [[Maybe Int]] -> IO (Either String [[Maybe Int]])
solveSudoku grid
    | not (checkSudoku grid) = return $ Left "Invalid Sudoku puzzle: Conflicting values"
    | otherwise = do
        solution <- solve grid
        case solution of
            Just sol -> return $ Right sol
            Nothing  -> return $ Left "No solution exists for this Sudoku puzzle"

solve :: [[Maybe Int]] -> IO (Maybe [[Maybe Int]])
solve grid = do
    -- Early conflict detection: if any empty cell has no possible candidates, the grid is unsolvable
    if not (isValidState grid)
        then return Nothing
        else if isSolved grid
            then return (Just grid)
            else case findBestEmptyCell grid of
                Nothing -> return Nothing
                Just (r, c, candidates) -> do
                    nums <- shuffleM candidates
                    tryValues grid r c nums

isValidState :: [[Maybe Int]] -> Bool
isValidState grid = all (all isCellValueValid) grid && all (\(r, c) -> not . null $ possibleValues grid r c) emptyCells && sizeConsistent grid
  where
    isCellValueValid :: Maybe Int -> Bool
    isCellValueValid Nothing    = True
    isCellValueValid (Just val) = val >= 1 && val <= 9

    emptyCells = [(r, c) | r <- [0..8], c <- [0..8], isNothing (grid !! r !! c)]

    sizeConsistent :: [[Maybe Int]] -> Bool
    sizeConsistent g = length g == 9 && all (\row -> length row == 9) g


isSolved :: [[Maybe Int]] -> Bool
isSolved grid = all (all isJust) grid && checkSudoku grid

-- Find the empty cell with the fewest possible candidates (MRV heuristic)
findBestEmptyCell :: [[Maybe Int]] -> Maybe (Int, Int, [Int])
findBestEmptyCell grid =
    if null sortedCells then Nothing else Just (r, c, candidates)
  where
    emptyCells = [(r, c) | r <- [0..8], c <- [0..8], isNothing (grid !! r !! c)]
    emptyCellsWithCandidates = [(r, c, possibleValues grid r c) | (r, c) <- emptyCells]
    validCells = filter (\(_, _, candidates) -> not (null candidates)) emptyCellsWithCandidates
    sortedCells = sortOn (\(_, _, candidates) -> length candidates) validCells
    (r, c, candidates) = head sortedCells

possibleValues :: [[Maybe Int]] -> Int -> Int -> [Int]
possibleValues grid r c =
    [1..9] \\ usedValues
  where
    usedValues = catMaybes $ getRow grid r ++ getCol grid c ++ getBox grid r c

tryValues :: [[Maybe Int]] -> Int -> Int -> [Int] -> IO (Maybe [[Maybe Int]])
tryValues _ _ _ [] = return Nothing
tryValues grid r c (v:vs) = do
    let newGrid = updateGrid grid r c (Just v)
    if not (checkSudoku newGrid)
        then tryValues grid r c vs
        else do
            result <- solve newGrid
            case result of
                Just solution -> return (Just solution)
                Nothing -> tryValues grid r c vs

getRow, getCol :: [[Maybe Int]] -> Int -> [Maybe Int]
getRow grid r = grid !! r
getCol grid c = map (!! c) grid

getBox :: [[Maybe Int]] -> Int -> Int -> [Maybe Int]
getBox grid r c =
    [grid !! r' !! c' | r' <- [rStart..rStart+2], c' <- [cStart..cStart+2]]
  where
    rStart = (r `div` 3) * 3
    cStart = (c `div` 3) * 3

toStrict :: TL.Text -> Text
toStrict = TL.toStrict