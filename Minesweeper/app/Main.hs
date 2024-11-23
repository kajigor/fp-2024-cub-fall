module Main (main) where

main :: IO ()
main = return () 






{-
module Main where

import Grid (Grid, Cell(..), initializeGrid)
import Control.Monad (forM_)

-- Print the grid to the console
printGrid :: Grid -> IO ()
printGrid grid = do
    forM_ grid $ \row -> do
        forM_ row $ \cell -> do
            putStr (cellToChar cell ++ " ")
        putStrLn ""

-- Convert a Cell to a character for display
cellToChar :: Cell -> String
cellToChar Hidden    = "■"   -- Hidden cells
cellToChar (Empty n) = show n  -- Revealed cells with numbers
cellToChar Mine      = "*"   -- Mines (for debugging purposes)


main :: IO ()
main = do
    putStrLn "Initializing Minesweeper grid..."
    grid <- initializeGrid 5 5 5 -- Example: 5x5 grid with 5 mines
    putStrLn "Generated grid (debug view):"
    printGrid grid-}