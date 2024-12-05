module ConsoleUI where

import Grid
import GameLogic
import ErrorHandling
import System.IO (hFlush, stdout)

-- Start the game
startGame :: IO ()
startGame = do
    putStrLn "Welcome to Minesweeper!"
    putStrLn "Choose difficulty: (e)asy, (m)edium, (h)ard"
    difficulty <- getLine
    case difficulty of
        "e" -> initializeAndStartGame 10 8 10
        "m" -> initializeAndStartGame 18 14 40
        "h" -> initializeAndStartGame 20 20 99
        _   -> putStrLn "Invalid choice." >> startGame

-- Initialize the grid and start the game loop
initializeAndStartGame :: Int -> Int -> Int -> IO ()
initializeAndStartGame rows cols mines = do
    grid <- initializeGrid rows cols mines Nothing
    gameLoop grid

-- Game loop to handle moves
gameLoop :: Grid -> IO ()
gameLoop grid = do
    printGrid grid
    putStr "Enter move (row col action [r/f]): "
    hFlush stdout
    input <- getLine
    case parseMove input of
        Just (r, c, 'r') ->
            let newGrid = revealCell grid (r, c)
            in if isLoss newGrid
               then putStrLn "Game Over!" >> printGrid newGrid
               else if isWin newGrid
                    then putStrLn "You win!" >> printGrid newGrid
                    else gameLoop newGrid
        Just (r, c, 'f') -> gameLoop (flagCell grid (r, c))
        _ -> putStrLn "Invalid input." >> gameLoop grid

-- Parse a move from input
parseMove :: String -> Maybe (Int, Int, Char)
parseMove input = case words input of
    [r, c, [action]] | action `elem` "rf" -> Just (read r, read c, action)
    _ -> Nothing

-- Print the grid to the console
printGrid :: Grid -> IO ()
printGrid = mapM_ (putStrLn . unwords . map cellToChar)

-- Convert a cell to displayable text
cellToChar :: (VisibleState, Cell) -> String
cellToChar (Hidden, _) = "#"
cellToChar (Flagged, _) = "F"
cellToChar (Revealed, Mine) = "*"
cellToChar (Revealed, Empty 0) = " "
cellToChar (Revealed, Empty n) = show n
