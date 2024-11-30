module Grid where

import System.Random (randomRIO)
import Data.List (nub)
import Control.Monad (replicateM)

-- Data type for each cell in the grid
data Cell = Mine
          | Empty Int
          | Hidden
          | Flagged
          deriving (Show, Eq)

-- Type alias for the game grid
type Grid = [[Cell]]

-- Initialize a grid with mines and calculate adjacent numbers
initializeGrid :: Int -> Int -> Int -> Maybe [(Int, Int)] -> IO Grid
initializeGrid rows cols mineCount minePositions = do
    minePositions' <- case minePositions of
        Just positions -> return positions
        Nothing -> placeMines rows cols mineCount
    let emptyGrid = replicate rows (replicate cols Hidden)
    let gridWithMines = addMines emptyGrid minePositions'
    return $ calculateNumbers gridWithMines


-- Place mines randomly on the grid
placeMines :: Int -> Int -> Int -> IO [(Int, Int)]
placeMines = generateUniquePositions

-- Add mines to the grid at the specified positions
addMines :: Grid -> [(Int, Int)] -> Grid
addMines = foldl placeMine
    where
        placeMine g (r, c) =
            take r g ++
            [take c (g !! r) ++ [Mine] ++ drop (c + 1) (g !! r)] ++
            drop (r + 1) g

-- Calculate numbers for all cells based on adjacent mines
calculateNumbers :: Grid -> Grid
calculateNumbers grid =
    [ [ if grid !! r !! c == Mine
            then Mine
            else Hidden
        | c <- [0..cols-1]
        ]
    | r <- [0..rows-1]
    ]
    where
        rows = length grid
        cols = length (head grid)

-- Count the number of mines adjacent to a cell
countAdjacentMines :: Grid -> Int -> Int -> Int
countAdjacentMines grid row col =
    length [() | (r, c) <- neighbors, isMine r c]
    where
        rows = length grid
        cols = length (head grid)
        neighbors = [(row + dr, col + dc) | dr <- [-1..1], dc <- [-1..1],(dr, dc)/=(0,0)]
        isMine r c = r >= 0 && r < rows && c >= 0 && c < cols && grid !! r !! c == Mine

-- Generate a list of unique random positions
generateUniquePositions :: Int -> Int -> Int -> IO [(Int, Int)]
generateUniquePositions rows cols count = do
    positions <- go []
    return $ take count positions
  where
    go acc
        | length acc >= count = return acc
        | otherwise = do
            pos <- randomPosition rows cols
            if pos `elem` acc
                then go acc
                else go (pos : acc)


-- Generate a Single random position
randomPosition :: Int -> Int -> IO (Int, Int)
randomPosition rows cols = do
    r <- randomRIO (0, rows - 1)
    c <- randomRIO (0, cols - 1)
    return (r, c)

-- Flags or unflags a cell
flagCell :: Grid -> (Int, Int) -> Grid
flagCell grid pos =
    case getCell grid pos of
        Just Hidden -> setCell grid pos Flagged
        Just Flagged -> setCell grid pos Hidden
        _ -> grid

-- Retrieves the cell at the given position
getCell :: Grid -> (Int, Int) -> Maybe Cell
getCell grid (r, c)
    | r >= 0 && r < length grid && c >= 0 && c < length (head grid) = Just (grid !! r !! c)
    | otherwise = Nothing

-- Sets the cell at the given position
setCell :: Grid -> (Int, Int) -> Cell -> Grid
setCell grid (r, c) cell =
    take r grid ++
    [take c (grid !! r) ++ [cell] ++ drop (c + 1) (grid !! r)] ++
    drop (r + 1) grid

-- Retrieves the list of adjacent cell positions
neighbors :: Grid -> (Int, Int) -> [(Int, Int)]
neighbors grid (row, col) =
    [(row + dr, col + dc) | dr <- [-1..1], dc <- [-1..1], (dr, dc) /= (0, 0), isValid (row + dr, col + dc)]
    where
        rows = length grid
        cols = length (head grid)
        isValid (r, c) = r >= 0 && r < rows && c >= 0 && c < cols
