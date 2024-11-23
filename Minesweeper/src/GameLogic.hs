module GameLogic where

import Grid (Grid, Cell(..))

-- Reveals a cell on the grid
revealCell :: Grid -> (Int, Int) -> Grid
revealCell = undefined

-- Checks if all non-mine cells are revealed
isWin :: Grid -> Bool
isWin = undefined

-- Checks if a mine is revealed
isLoss :: Grid -> Bool
isLoss = undefined

-- Flags or unflags a cell
flagCell :: Grid -> (Int, Int) -> Grid
flagCell = undefined
