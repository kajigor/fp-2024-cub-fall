module ErrorHandling where

import Grid

-- Error types for Minesweeper
data GameError
  = InvalidMove String       -- For invalid user moves
  | InvalidInput String      -- For invalid input parsing
  | InitializationError String -- For errors during game setup
  deriving (Show, Eq)

-- Validate a move based on the grid size
validateMove :: Grid -> (Int, Int) -> Either GameError ()
validateMove grid (row, col)
  | row < 0 || col < 0 = Left $ InvalidMove "Move is out of bounds (negative indices)."
  | row >= numRows || col >= numCols = Left $ InvalidMove "Move is out of bounds (indices exceed grid dimensions)."
  | otherwise = Right ()
  where
    numRows = length grid
    numCols = if null grid then 0 else length (head grid)

-- Validate grid dimensions during initialization
validateGridDimensions :: Int -> Int -> Either GameError ()
validateGridDimensions rows cols
  | rows <= 0 || cols <= 0 = Left $ InitializationError "Grid dimensions must be positive integers."
  | otherwise = Right ()

-- Validate the number of mines during initialization
validateMineCount :: Int -> Int -> Int -> Either GameError ()
validateMineCount rows cols mines
  | mines < 0 = Left $ InitializationError "Number of mines cannot be negative."
  | mines > rows * cols = Left $ InitializationError "Number of mines exceeds grid capacity."
  | otherwise = Right ()

-- Centralized error handler for user-friendly output
handleError :: GameError -> String
handleError (InvalidMove msg) = "Invalid move: " ++ msg
handleError (InvalidInput msg) = "Invalid input: " ++ msg
handleError (InitializationError msg) = "Initialization error: " ++ msg
