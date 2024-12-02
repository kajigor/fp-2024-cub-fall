-- Define the Grid.Operation module for revealing and flagging
module Grid.Operation
    ( revealCell
    , flagCell
    , isOutOfBounds
    , getNeighborCoords
    ) where

import Grid.Core (Grid)
import Cell (Cell(..))
import qualified Data.Set as S
import qualified Data.Vector as V

-- Check for out-of-bounds indices
isOutOfBounds :: Int -> Int -> Grid -> Bool
isOutOfBounds row col grid =
    row < 0 || row >= V.length grid || col < 0 || col >= V.length (grid V.! 0)

-- Update a Cell in the grid
setCell :: Int -> Int -> Cell -> Grid -> Grid
setCell row col newCell grid =
    grid V.// [(row, rowCells V.// [(col, newCell)])]
  where
    rowCells = grid V.! row

-- Get neighbor coordinates
getNeighborCoords :: Int -> Int -> Grid -> [(Int, Int)]
getNeighborCoords row col grid =
    [ (r, c)
    | dr <- [-1, 0, 1]
    , dc <- [-1, 0, 1]
    , (dr, dc) /= (0, 0)                   -- Exclude the current cell
    , let r = row + dr
    , let c = col + dc
    , not (isOutOfBounds r c grid)
    ]

-- Cascading reveal logic
revealCell :: Int -> Int -> Grid -> Grid
revealCell row col grid = fst $ revealCellHelper row col grid S.empty

revealCellHelper :: Int -> Int -> Grid -> S.Set (Int, Int) -> (Grid, S.Set (Int, Int))
revealCellHelper row col grid visited
    | isOutOfBounds row col grid = (grid, visited)
    | (row, col) `S.member` visited = (grid, visited)
    | otherwise =
        let cell = grid V.! row V.! col
            visited' = S.insert (row, col) visited
            updatedCell = cell { isRevealed = True }
            updatedGrid = setCell row col updatedCell grid
        in if isMine cell || adjacentMines cell > 0
           then (updatedGrid, visited')
           else
               foldl
                   (\(g, v) (r, c) ->
                       let (g', v') = revealCellHelper r c g v
                       in (g', v'))
                   (updatedGrid, visited')
                   (getNeighborCoords row col grid)

-- Toggle flag on a cell
flagCell :: Int -> Int -> Grid -> Grid
flagCell row col grid =
    setCell row col updatedCell grid
  where
    cell = grid V.! row V.! col
    updatedCell = cell { isFlagged = not (isFlagged cell) }