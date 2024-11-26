{-# LANGUAGE LambdaCase #-}

module GameLogic where

import Grid
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


-- Reveals all adjacent cells if there are no mines adjacent to the cell
revealAdjacent :: Grid -> (Int, Int) -> Grid
revealAdjacent grid pos =
    let grid' = setCell grid pos (fromMaybe Hidden (getCell grid pos))
        adjacentCells = neighbors grid pos
    in if any (\p -> case getCell grid p of
                       Just Mine -> True
                       _ -> False) adjacentCells
       then grid'
       else foldl revealCell grid' adjacentCells


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
