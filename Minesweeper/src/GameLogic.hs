{-# LANGUAGE LambdaCase #-}

module GameLogic where

import Grid
import ErrorHandling
import Data.Maybe (fromMaybe)

-- Reveals a cell on the grid
revealCell :: Grid -> (Int, Int) -> Grid
revealCell grid pos = 
    case getCell grid pos of
        Just Hidden ->
            let adjacentMines = uncurry (countAdjacentMines grid) pos
                newGrid = setCell grid pos (Empty adjacentMines)
            in if adjacentMines == 0
               then foldl revealCell newGrid (neighbors grid pos)
               else newGrid
        _ -> grid

-- Checks if all non-mine cells are revealed
isWin :: Grid -> Bool
isWin = all
        (all
            (\case
                Mine -> True
                Empty _ -> True
                Hidden -> False))

-- Checks if a mine is revealed
isLoss :: Grid -> Bool
isLoss = any
        (any
            (\case
                Mine -> True
                Empty _ -> False
                Hidden -> False
                Flagged -> False))

-- Retrieves the list of adjacent cell positions
neighbors :: Grid -> (Int, Int) -> [(Int, Int)]
neighbors grid (row, col) =
    [(row + dr, col + dc) | dr <- [-1..1], dc <- [-1..1], (dr, dc) /= (0, 0), isValid (row + dr, col + dc)]
    where
        rows = length grid
        cols = length (head grid)
        isValid (r, c) = r >= 0 && r < rows && c >= 0 && c < cols

playMove :: Grid -> (Int, Int) -> Either GameError Grid
playMove grid (row, col) =
  case validateMove grid (row, col) of
    Left err -> Left err  -- Return the error
    Right () -> Right (revealCell grid (row, col))  -- Proceed if valid
