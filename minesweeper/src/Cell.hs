-- define module Cell and export
module Cell
    ( Cell(..)
    , emptyCell
    ) where

-- Define the Cell data type
data Cell = Cell
    { isMine        :: !Bool  -- Whether the cell contains a mine
    , isRevealed    :: !Bool  -- Whether the cell is revealed
    , isFlagged     :: !Bool  -- Whether the cell is flagged
    , adjacentMines :: !Int   -- Number of adjacent mines
    } deriving (Show, Eq)

-- Create an empty cell with default values
emptyCell :: Cell
emptyCell = Cell 
    { isMine = False
    , isRevealed = False
    , isFlagged = False
    , adjacentMines = 0
    }