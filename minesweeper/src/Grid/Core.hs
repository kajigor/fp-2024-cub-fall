-- Define the Grid module for core functionality and rendering
module Grid.Core
    ( Grid
    , renderGrid
    ) where

import Cell (Cell(..))

-- Define the Grid type as a 2D list of Cell
type Grid = [[Cell]]

-- Display the grid for the game on the terminal
renderGrid :: Grid -> IO ()
renderGrid grid = do
    putStrLn $ "    " ++ unwords (map (pad . show) [0 .. cols - 1]) -- Column indices
    putStrLn $ "  " ++ replicate horizontalLineLength '-'           -- Top border
    mapM_ renderRow (zip ([0 ..] :: [Int]) grid) -- Had a warning because mapM_ can't infer the type of the list being passed to it. That's why the list passed to zip is explicitly Int
  where
    cols = length (head grid)                                       -- Number of columns
    horizontalLineLength = (cols * 4) + 4                          -- Corrected horizontal line length
    pad s = replicate (3 - length s) ' ' ++ s                      -- Padding for alignment

    -- Render a single row of the grid
    renderRow (rowIndex, row) = do
        putStrLn $ pad (show rowIndex) ++ " | " ++ concatMap renderCell row ++ "|" -- Row with index and cells
        putStrLn $ "  " ++ replicate horizontalLineLength '-'                     -- Row border

    -- Render a single cell of the grid
    renderCell cell
        | isRevealed cell && isMine cell  = "*   " -- Revealed mine
        | isRevealed cell                 = show (adjacentMines cell) ++ "   "
        | isFlagged cell                  = "F   " -- Flagged cell
        | otherwise                       = ".   " -- Unrevealed cell