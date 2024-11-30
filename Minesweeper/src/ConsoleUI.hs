module ConsoleUI where

import Grid
import GameLogic
import Control.Monad (forever)
import System.IO (hFlush, stdout)

-- Main function to start the game
startGame :: IO ()
startGame = do
    putStrLn "Welcome to Minesweeper!"
    putStrLn "Choose difficulty level: (e)asy, (m)edium, (h)ard"
    difficulty <- getLine
    case difficulty of
        "e" -> initializeAndStartGame 10 8 10
        "m" -> initializeAndStartGame 18 14 40
        "h" -> initializeAndStartGame 20 20 99
        _   -> do
            putStrLn "Invalid choice. Please enter e, m, or h."
            startGame

-- Initialize the grid and start the game loop
initializeAndStartGame :: Int -> Int -> Int -> IO ()
initializeAndStartGame width height mines = do
    grid <- initializeGrid height width mines Nothing
    gameLoop grid

-- Game loop to handle user input and update the grid
gameLoop :: Grid -> IO ()
gameLoop grid = do
    printGrid grid
    putStrLn "Enter your move (row col action):"
    move <- getLine
    case parseMove move of
        Just (r, c, action) -> do
            let newGrid = case action of
                            'r' -> revealCell grid (r, c)
                            'f' -> flagCell grid (r, c)
                            _   -> grid
            if isWin newGrid
                then do
                    putStrLn "Congratulations! You've won!"
                    printGrid newGrid
                else if isLoss newGrid
                    then do
                        putStrLn "Game Over! You've hit a mine!"
                        printGrid newGrid
                    else gameLoop newGrid
        Nothing -> do
            putStrLn "Invalid move. Please enter a valid move."
            gameLoop grid

-- Parse the grid dimensions
parseDimensions :: String -> Maybe (Int, Int, Int)
parseDimensions input =
    case map reads (words input) of
        [[(rows, "")], [(cols, "")], [(mines, "")]] 
            | rows > 0 && cols > 0 && mines > 0 -> Just (rows, cols, mines)
        _ -> Nothing

-- Parse the user's move
parseMove :: String -> Maybe (Int, Int, Char)
parseMove input =
    case words input of
        [rs, cs, [action]] | action `elem` ['r', 'f'] ->
            case (reads rs, reads cs) of
                ([(r, "")], [(c, "")]) -> Just (r, c, action)
                _ -> Nothing
        _ -> Nothing

-- Print the grid to the console
printGrid :: Grid -> IO ()
printGrid grid = do
    mapM_ (putStrLn . unwords . map cellToChar) grid

-- Convert a Cell to a displayable string
cellToChar :: Cell -> String
cellToChar Hidden    = "■"  -- Hidden cells
cellToChar Flagged   = "F"  -- Flagged cells
cellToChar (Empty 0) = " "  -- Empty cells with no adjacent mines
cellToChar (Empty n) = show n  -- Revealed cells with adjacent mines
cellToChar Mine      = "*"  -- Mine (shown when game is over)
