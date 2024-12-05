-- Define the Grid module for core functionality and rendering
module Grid.Core
    ( Grid
    , renderGrid
    ) where

import Cell (Cell(..))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- Define the Grid type as a 2D vector of Cell
type Grid = V.Vector (V.Vector Cell)

-- Display the grid for the game on the terminal
renderGrid :: Grid -> IO ()
renderGrid grid = do
    putStrLn $ "    " ++ unwords (map (pad . show) [0 .. cols - 1]) -- Column indices
    putStrLn $ "  " ++ replicate horizontalLineLength '-'           -- Top border
    V.imapM_ renderRow grid
  where
    cols = V.length (V.head grid)                                   -- Number of columns
    horizontalLineLength = (cols * 4) + 4                          -- Corrected horizontal line length
    pad s = replicate (3 - length s) ' ' ++ s                      -- Padding for alignment

    -- Render a single row of the grid
    renderRow :: Int -> V.Vector Cell -> IO ()
    renderRow rowIndex row = do
        putStrLn $ pad (show rowIndex) ++ " | " ++ V.foldl' (\acc cell -> acc ++ renderCell cell) "" row ++ "|"
        putStrLn $ "  " ++ replicate horizontalLineLength '-'       -- Row border

    -- Render a single cell of the grid
    renderCell :: Cell -> String
    renderCell cell
        | isRevealed cell && isMine cell  = "*   " -- Revealed mine
        | isRevealed cell                 = show (adjacentMines cell) ++ "   "
        | isFlagged cell                  = "F   " -- Flagged cell
        | otherwise                       = ".   " -- Unrevealed cell