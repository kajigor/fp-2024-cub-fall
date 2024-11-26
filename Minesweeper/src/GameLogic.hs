{-# LANGUAGE LambdaCase #-}

module GameLogic where

import Grid
import Data.Maybe (fromMaybe)

-- Reveals a cell on the grid
revealCell :: Grid -> (Int, Int) -> Grid
revealCell grid pos = case getCell grid pos of
    Just Hidden ->
        let newGrid = setCell grid pos (Empty (uncurry (countAdjacentMines grid) pos))
        in if uncurry (countAdjacentMines grid) pos == 0
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
