-- Define the Grid.Generation module for grid creation
module Grid.Generation
    ( generateEmptyGrid
    , generateRandomGrid
    ) where

import qualified Data.Set as Set
import System.Random (randomRIO)
import Data.List.Split (chunksOf)
import Cell (Cell(..), emptyCell)
import Grid.Core (Grid)

-- Generate an empty 2D grid
generateEmptyGrid :: Int -> Int -> Grid
generateEmptyGrid rows cols = replicate rows (replicate cols emptyCell)

-- Helper function to generate unique indices excluding the cell that a player chooses at the beginning
randomUniqueIndicesExcluding :: Int -> Int -> [Int] -> IO [Int]
randomUniqueIndicesExcluding n maxIndex exclude = go Set.empty
  where
    excludeSet = Set.fromList exclude
    go indices
      | Set.size indices == n = return (Set.toList indices) -- Done when we have n unique indices. Convert set to a list
      | otherwise = do
          newIndex <- randomRIO (0, maxIndex - 1) -- random index generation
          if newIndex `Set.member` indices || newIndex `Set.member` excludeSet
            then go indices
            else go (Set.insert newIndex indices) -- Add the new index to the set

-- Get neighbors of a cell (handles edge cases)
getNeighbors :: Int -> Int -> Int -> [Int]
getNeighbors index rows cols =
  [ idx
  | (dr, dc) <- [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)] -- relative offsets of cells in the neighborhood
  , let r = index `div` cols + dr -- calculating a row of the cell
  , let c = index `mod` cols + dc -- calculating a column of a cell
  , r >= 0, r < rows, c >= 0, c < cols -- filtering invalid neighbors
  , let idx = r * cols + c -- convert neighbor's coordinates back to a single index
  ]

-- Count mines around a specific cell
countAdjacentMines :: [Cell] -> Int -> Int -> Int -> Int
countAdjacentMines flatGrid rows cols index =
  length [ () | neighbor <- getNeighbors index rows cols, isMine (flatGrid !! neighbor) ]

-- Update each cell with the count of adjacent mines
calculateAdjacency :: [Cell] -> Int -> Int -> [Cell]
calculateAdjacency flatGrid rows cols =
  [ if isMine cell then cell
    else cell { adjacentMines = countAdjacentMines flatGrid rows cols index }
  | (cell, index) <- zip flatGrid [0 ..]
  ]

-- Finally generate random grid
-- List is turned back into 2D
generateRandomGrid :: Int -> Int -> Int -> (Int, Int) -> IO Grid
generateRandomGrid rows cols numMines (firstRow, firstCol) = do
  let totalCells = rows * cols

  -- Calculate indices to exclude (first cell and neighbors)
  let excludeIndices = (firstRow * cols + firstCol) : getNeighbors (firstRow * cols + firstCol) rows cols
  -- Generate random mine locations 
  mineIndices <- randomUniqueIndicesExcluding numMines totalCells excludeIndices

  -- Create a flat list of cells with mines by replacing mineIndices with mines
  -- A flat 1D list is necessary to simplify accessing cells and to use functions like zip, map, and filter (in the functions I have been only using zip)
  let flatGrid = [if i `elem` mineIndices then emptyCell { isMine = True } else emptyCell | i <- [0 .. totalCells - 1]]

  -- Calculate adjacent mines for each cell
  let flatGridWithCounters = calculateAdjacency flatGrid rows cols

  -- Convert flat list back to 2D grid. chunksOf to split list into smaller sublists
  return $ chunksOf cols flatGridWithCounters