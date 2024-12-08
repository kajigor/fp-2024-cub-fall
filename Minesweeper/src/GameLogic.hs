{-# LANGUAGE LambdaCase #-}

module GameLogic where

import Grid
import ErrorHandling

-- Reveal a cell on the grid
revealCell :: Grid -> (Int, Int) -> Grid
revealCell grid pos =
    case getCell grid pos of
        Just (Hidden, truthCell) ->
            let newGrid = setCell grid pos (Revealed, truthCell)
            in if truthCell == Empty 0
               then foldl revealCell newGrid (neighbors grid pos)
               else newGrid
        _ -> grid

-- Check if all non-mine cells are revealed
isWin :: Grid -> Bool
isWin = all (all (\case
    (Revealed, Mine) -> False           -- Revealed mines lead to loss
    (Revealed, _) -> True                -- Revealed non-mine cells are okay
    (Hidden, Mine) -> True               -- Hidden mines are considered "safe" for the win condition
    (Flagged, _) -> True                 -- Flagged cells are considered correctly handled
    _ -> False                           -- Any other state is not a valid win
  ))

-- Check if a mine is revealed
isLoss :: Grid -> Bool
isLoss = any (any (\case (Revealed, Mine) -> True; _ -> False))

-- Retrieve the list of adjacent cell positions
neighbors :: Grid -> (Int, Int) -> [(Int, Int)]
neighbors grid (row, col) =
    [(row + dr, col + dc) | dr <- [-1..1], dc <- [-1..1], (dr, dc) /= (0, 0), isValid (row + dr, col + dc)]
    where
        rows = length grid
        cols = length (head grid)
        isValid (r, c) = r >= 0 && r < rows && c >= 0 && c < cols

playMove :: Grid -> (Int, Int, Char) -> Either GameError Grid
playMove grid (row, col, action) =
    case validateMove grid (row, col) of
        Left err -> Left err  -- Invalid move
        Right () ->
            case action of
                'r' -> -- Reveal action
                    let newGrid = revealCell grid (row, col)
                    in if isLoss newGrid
                       then Left $ InvalidMove "You hit a mine! Game over."
                       else Right newGrid
                'f' -> -- Flag action
                    Right $ flagCell grid (row, col)
                _ -> Left $ InvalidInput "Invalid action. Use 'r' to reveal or 'f' to flag."

calculateMinesLeft :: Grid -> Int
calculateMinesLeft grid = totalMines - flaggedCells
  where
    totalMines = countMines grid
    flaggedCells = length [(vs, c) | row <- grid, (vs, c) <- row, vs == Flagged]

countMines :: Grid -> Int
countMines = sum . map (length . filter (\(_, cell) -> cell == Mine))