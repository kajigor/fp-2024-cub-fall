-- Define the Grid.Operations module for revealing and flagging
module Grid.Operation
    ( revealCell
    , flagCell
    , isOutOfBounds
    ) where

import Grid.Core (Grid)
import Cell (Cell(..))

-- Check for out of bounds. Used for both revealing and flagging cells. Also in generation functions
isOutOfBounds :: Int -> Int -> Grid -> Bool
isOutOfBounds row col grid =
    row < 0 || row >= length grid || col < 0 || col >= length (head grid)

-- Update a Cell
setCell :: Int -> Int -> Cell -> Grid -> Grid
setCell row col newCell grid =
    take row grid ++ [take col rowCells ++ [newCell] ++ drop (col + 1) rowCells] ++ drop (row + 1) grid
  where
    rowCells = grid !! row

-- Get neighbor coordinates
getNeighborCoords :: Int -> Int -> Grid -> [(Int, Int)]
getNeighborCoords row col grid =
    [ (r, c)
    | (dr, dc) <- [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    , let r = row + dr
    , let c = col + dc
    , not (isOutOfBounds r c grid)
    ]

-- Cascading reveal logic
revealCell :: Int -> Int -> Grid -> Grid
revealCell row col grid
    | isOutOfBounds row col grid = grid -- Ignore out-of-bounds clicks
    | isRevealed cell = grid            -- Skip already revealed cells
    | isMine cell = setCell row col (cell { isRevealed = True }) grid -- Reveal the mine
    | adjacentMines cell > 0 = setCell row col (cell { isRevealed = True }) grid -- Reveal numbers without cascading
    | otherwise = foldl (\g (r, c) -> revealCell r c g) updatedGrid neighbors
  where
    cell = grid !! row !! col
    updatedGrid = setCell row col (cell { isRevealed = True }) grid -- Mark the current cell as revealed
    neighbors = getNeighborCoords row col grid


flagCell :: Int -> Int -> Grid -> Grid
flagCell row col grid =
    let cell = grid !! row !! col
        updatedCell = cell { isFlagged = not (isFlagged cell) }
    in setCell row col updatedCell grid