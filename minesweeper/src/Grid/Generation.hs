module Grid.Generation
    ( generateEmptyGrid
    , generateRandomGrid
    ) where

import qualified Data.Set as Set
import System.Random (randomRIO)
import qualified Data.Vector as V
import Cell (Cell(..), emptyCell)
import Grid.Core (Grid)
import GHC.Utils.Misc (count)

-- Generate an empty grid
generateEmptyGrid :: Int -> Int -> Grid
generateEmptyGrid rows cols = V.replicate rows (V.replicate cols emptyCell)

-- Helper function to generate unique indices excluding the cell that a player chooses at the beginning
randomUniqueIndicesExcluding :: Int -> Int -> [Int] -> IO [Int]
randomUniqueIndicesExcluding n maxIndex exclude = go Set.empty
  where
    excludeSet = Set.fromList exclude
    go indices
      | Set.size indices == n = return (Set.toList indices)
      | Set.size indices + Set.size excludeSet >= maxIndex = error "Not enough cells to place the required number of mines."
      | otherwise = do
          newIndex <- randomRIO (0, maxIndex - 1)
          if newIndex `Set.member` indices || newIndex `Set.member` excludeSet
            then go indices
            else go (Set.insert newIndex indices)

-- Get neighbors of a cell (handles edge cases)
getNeighbors :: Int -> Int -> Int -> [Int]
getNeighbors index rows cols =
  [ idx
  | (dr, dc) <- [(-1, -1), (-1, 0), (-1, 1),
                 (0, -1),           (0, 1),
                 (1, -1),  (1, 0),  (1, 1)]
  , let r = index `div` cols + dr
  , let c = index `mod` cols + dc
  , r >= 0, r < rows, c >= 0, c < cols
  , let idx = r * cols + c
  ]

-- Count mines around a specific cell
countAdjacentMines :: V.Vector Cell -> Int -> Int -> Int -> Int
countAdjacentMines flatGrid rows cols index =
  count (\neighbor -> isMine (flatGrid V.! neighbor)) (getNeighbors index rows cols)

-- Update each cell with the count of adjacent mines
calculateAdjacency :: V.Vector Cell -> Int -> Int -> V.Vector Cell
calculateAdjacency flatGrid rows cols =
  V.imap
    (\index cell ->
       if isMine cell
         then cell
         else cell { adjacentMines = countAdjacentMines flatGrid rows cols index })
    flatGrid

-- Generate a random grid with mines
generateRandomGrid :: Int -> Int -> Int -> (Int, Int) -> IO Grid
generateRandomGrid rows cols numMines (firstRow, firstCol)
    | numMines <= 0 = error "Number of mines must be greater than 0."
    | numMines >= totalCells = error "Number of mines must be less than the total number of cells."
    | numMines > validCells = error $ "Too many mines: Cannot place " ++ show numMines ++
                                      " mines in a grid with only " ++ show validCells ++ " valid cells."
    | otherwise = do
        mineIndices <- randomUniqueIndicesExcluding numMines totalCells excludeIndices

        -- Create a flat vector of cells with mines
        let flatGrid = V.generate totalCells (\i -> if i `elem` mineIndices
                                                    then emptyCell { isMine = True }
                                                    else emptyCell)

        -- Calculate adjacent mines for each cell
        let flatGridWithCounters = calculateAdjacency flatGrid rows cols

        -- Convert flat vector back to a 2D grid
        return $ V.generate rows (\r -> V.slice (r * cols) cols flatGridWithCounters)
  where
    totalCells = rows * cols
    excludeIndices = (firstRow * cols + firstCol) : getNeighbors (firstRow * cols + firstCol) rows cols
    validCells = totalCells - length (Set.fromList excludeIndices)