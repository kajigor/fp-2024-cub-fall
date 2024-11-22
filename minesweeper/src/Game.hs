module Game
    ( gameLoop
    , parseInput
    , checkWin
    , askFirstMove
    , validateParameters
    , getGameParameters
    ) where

import Grid.Core (Grid, renderGrid)
import Grid.Generation (generateRandomGrid)
import Grid.Operation (revealCell, flagCell, isOutOfBounds)
import Cell (isMine, isRevealed)
import Data.List.Split (splitOn)

-- Get game parameters (rows, columns, mines) in a single line
getGameParameters :: IO (Either String (Int, Int, Int))
getGameParameters = do
    putStrLn "Enter grid dimensions and number of mines (e.g., rows,cols,mines):"
    input <- getLine
    case parseGameParameters input of
        Just (rows, cols, mines) -> 
            case validateParameters rows cols mines of
                Left err -> return $ Left err
                Right () -> return $ Right (rows, cols, mines)
        Nothing -> return $ Left "Invalid input format. Use 'rows,cols,mines'."

-- Parse the input string for game parameters
parseGameParameters :: String -> Maybe (Int, Int, Int)
parseGameParameters input =
    case map readMaybe (splitOn "," input) of
        [Just rows, Just cols, Just mines] -> Just (rows, cols, mines)
        _ -> Nothing

-- Safely parse an integer
readMaybe :: String -> Maybe Int
readMaybe s = case reads s of
    [(n, "")] -> Just n
    _         -> Nothing

-- Validate game parameters
validateParameters :: Int -> Int -> Int -> Either String ()
validateParameters rows cols mines
    | rows <= 0 || cols <= 0 = Left "Grid dimensions must be greater than 0."
    | mines <= 0 = Left "Number of mines must be greater than 0."
    | mines >= rows * cols = Left "Number of mines must be less than the total number of cells."
    | otherwise = Right ()

-- Ask the user for their first move
askFirstMove :: Grid -> Int -> Int -> Int -> IO (Either String (Int, Int, Grid))
askFirstMove emptyGrid rows cols mines = do
    renderGrid emptyGrid
    putStrLn "\nEnter your first move (row,column):"
    input <- getLine
    case parseInput input of
        Just (False, firstRow, firstCol) ->
            if isOutOfBounds firstRow firstCol emptyGrid
                then return $ Left "Invalid move: Cell is out of bounds."
                else do
                    grid <- generateRandomGrid rows cols mines (firstRow, firstCol)
                    let updatedGrid = revealCell firstRow firstCol grid
                    return $ Right (firstRow, firstCol, updatedGrid)
        _ -> return $ Left "Invalid input format. Use 'row,column'."

-- Game loop to play Minesweeper
gameLoop :: Grid -> Int -> Int -> IO ()
gameLoop grid rows cols = do
    renderGrid grid  -- Display the current state of the grid
    putStrLn "\nEnter your move (e.g., 'row,column' to reveal or 'F row,column' to flag):"
    input <- getLine

    case parseInput input of
        Just (isFlag, row, col) ->
            if isOutOfBounds row col grid
                then do
                    putStrLn "Invalid move: Cell is out of bounds. Try again!"
                    gameLoop grid rows cols
                else do
                    let updatedGrid = if isFlag then flagCell row col grid else revealCell row col grid
                    if isFlag
                        then gameLoop updatedGrid rows cols
                        else checkGameState updatedGrid row col rows cols
        Nothing -> do
            putStrLn "Invalid input format. Try again!"
            gameLoop grid rows cols

-- Check game state and continue or end the game
checkGameState :: Grid -> Int -> Int -> Int -> Int -> IO ()
checkGameState grid row col rows cols
    | isMine (grid !! row !! col) = do
        putStrLn "\nBOOM! You hit a mine. Game Over!"
        renderGrid grid
    | checkWin grid = do
        putStrLn "\nCongratulations! You cleared the grid!"
        renderGrid grid
    | otherwise = gameLoop grid rows cols

-- Parse input of the form "r,c" or "F r,c"
parseInput :: String -> Maybe (Bool, Int, Int)
parseInput input =
    case input of
        ('F' : ' ' : rest) -> parseCoords rest >>= \(row, col) -> Just (True, row, col)  -- Flagging input
        coords -> parseCoords coords >>= \(row, col) -> Just (False, row, col)          -- Revealing input
  where
    parseCoords str = case span (/= ',') str of
        (rowStr, ',' : colStr) ->
            case (reads rowStr, reads colStr) of
                ((row, _) : _, (col, _) : _) -> Just (row, col)
                _ -> Nothing
        _ -> Nothing

-- Check if the player has won (all non-mine cells are revealed)
checkWin :: Grid -> Bool
checkWin = all (all (\cell -> isMine cell || isRevealed cell))

-- Utility functions to extract tuple elements
fst3 (x, _, _) = x
snd3 (_, y, _) = y
thd3 (_, _, z) = z