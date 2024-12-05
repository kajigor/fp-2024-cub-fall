module ErrorHandling where

import Grid
import Text.Read (readMaybe)

-- Define the types of game errors
data GameError
    = InvalidMove String        -- Error for invalid moves
    | InvalidInput String       -- Error for invalid input parsing
    | InitializationError String -- Error for grid initialization issues
    deriving (Show, Eq)

-- Validate a move made by the player
validateMove :: Grid -> (Int, Int) -> Either GameError ()
validateMove grid (row, col)
    | row < 0 || col < 0 = Left $ InvalidMove "Move is out of bounds (negative indices)."
    | row >= length grid || col >= length (head grid) = Left $ InvalidMove "Move is out of bounds (exceeds grid size)."
    | case getCell grid (row, col) of
        Nothing -> True -- Invalid position
        Just (Revealed, _) -> True -- Can't move on revealed cells
        Just (Flagged, _) -> True -- Can't move on flagged cells
        _ -> False = Left $ InvalidMove "Move is invalid (cell already revealed or flagged)."
    | otherwise = Right ()

-- Validate input parsing
validateInput :: String -> Either GameError (Int, Int, Char)
validateInput input =
    case words input of
        [r, c, [action]]
            | action `elem` "rf" -> 
                case (readMaybe r, readMaybe c) of
                    (Just row, Just col) -> Right (row, col, action)
                    _ -> Left $ InvalidInput "Row and column must be integers."
            | otherwise -> Left $ InvalidInput "Action must be 'r' (reveal) or 'f' (flag)."
        _ -> Left $ InvalidInput "Input must be in the format: row col action."

-- Validate grid initialization
validateGridInitialization :: Int -> Int -> Int -> Either GameError ()
validateGridInitialization rows cols mines
    | rows <= 0 || cols <= 0 = Left $ InitializationError "Grid dimensions must be positive integers."
    | mines < 0 = Left $ InitializationError "Number of mines cannot be negative."
    | mines >= rows * cols = Left $ InitializationError "Number of mines exceeds or equals the total number of cells."
    | otherwise = Right ()
