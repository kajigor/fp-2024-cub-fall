module Grid where

import System.Random (randomRIO)
import Data.List (nub)
import Control.Monad (replicateM)

-- Data type for each cell in the grid
data Cell = Mine | Empty Int deriving (Show, Eq)
data VisibleState = Hidden | Revealed | Flagged deriving (Show, Eq)

-- Type alias for the game grid: (Visible state, True value)
type Grid = [[(VisibleState, Cell)]]

-- Initialize a grid with mines and calculate adjacent numbers
initializeGrid :: Int -> Int -> Int -> Maybe [(Int, Int)] -> IO Grid
initializeGrid rows cols mineCount minePositions = do
    minePositions' <- case minePositions of
        Just positions -> return positions
        Nothing -> placeMines rows cols mineCount
    let truthGrid = calculateNumbers $ addMines (replicate rows (replicate cols (Empty 0))) minePositions'
    return $ map (map (\trueCell -> (Hidden, trueCell))) truthGrid

-- Place mines randomly on the grid
placeMines :: Int -> Int -> Int -> IO [(Int, Int)]
placeMines = generateUniquePositions

-- Add mines to the grid at the specified positions
addMines :: [[Cell]] -> [(Int, Int)] -> [[Cell]]
addMines = foldl placeMine
    where
        placeMine g (r, c) =
            take r g ++
            [take c (g !! r) ++ [Mine] ++ drop (c + 1) (g !! r)] ++
            drop (r + 1) g

-- Calculate numbers for all cells based on adjacent mines
calculateNumbers :: [[Cell]] -> [[Cell]]
calculateNumbers grid =
    [ [ if cell == Mine then Mine else Empty (countAdjacentMines grid r c)
        | (c, cell) <- zip [0..] row ]
      | (r, row) <- zip [0..] grid ]

-- Count the number of mines adjacent to a cell
countAdjacentMines :: [[Cell]] -> Int -> Int -> Int
countAdjacentMines grid row col =
    length [() | (r, c) <- neighbors, isMine r c]
    where
        rows = length grid
        cols = length (head grid)
        neighbors = [(row + dr, col + dc) | dr <- [-1..1], dc <- [-1..1], (dr, dc) /= (0, 0)]
        isMine r c = r >= 0 && r < rows && c >= 0 && c < cols && grid !! r !! c == Mine

-- Generate a list of unique random positions
generateUniquePositions :: Int -> Int -> Int -> IO [(Int, Int)]
generateUniquePositions rows cols count = go []
  where
    go acc
        | length acc >= count = return acc
        | otherwise = do
            pos <- randomPosition rows cols
            if pos `elem` acc then go acc else go (pos : acc)

-- Generate a random position within the grid
randomPosition :: Int -> Int -> IO (Int, Int)
randomPosition rows cols = do
    r <- randomRIO (0, rows - 1)
    c <- randomRIO (0, cols - 1)
    return (r, c)

-- Reveal a cell and recursively reveal adjacent cells if the cell is empty
flagCell :: Grid -> (Int, Int) -> Grid
flagCell grid pos =
    case getCell grid pos of
        Just (Hidden, cell) -> setCell grid pos (Flagged, cell)
        Just (Flagged, cell) -> setCell grid pos (Hidden, cell)
        _ -> grid

-- Reveal a cell and recursively reveal adjacent cells if the cell is empty
getCell :: Grid -> (Int, Int) -> Maybe (VisibleState, Cell)
getCell grid (r, c)
    | r >= 0 && r < length grid && c >= 0 && c < length (head grid) = Just (grid !! r !! c)
    | otherwise = Nothing

-- Reveal a cell and recursively reveal adjacent cells if the cell is empty
setCell :: Grid -> (Int, Int) -> (VisibleState, Cell) -> Grid
setCell grid (r, c) cell =
    take r grid ++
    [take c (grid !! r) ++ [cell] ++ drop (c + 1) (grid !! r)] ++
    drop (r + 1) grid
