module Game
    ( gameLoop
    , parseInput
    , checkWin
    , askFirstMove
    , validateParameters
    , getGameParameters
    , processMove
    ) where

import qualified Data.Vector as V
import Grid.Core (Grid, renderGrid)
import Grid.Generation (generateRandomGrid, generateEmptyGrid)
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
        Nothing -> return $ Left "Invalid input format. Please use 'rows,cols,mines'."

-- Parse the input string for game parameters
parseGameParameters :: String -> Maybe (Int, Int, Int)
parseGameParameters input =
    case map (readMaybe . trim) (splitOn "," input) of
        [Just rows, Just cols, Just mines] -> Just (rows, cols, mines)
        _ -> Nothing

-- Trim leading and trailing whitespace
trim :: String -> String
trim = unwords . words

-- Safely parse an integer
readMaybe :: String -> Maybe Int
readMaybe s = case reads s of
    [(n, "")] -> Just n
    _         -> Nothing

-- Validate game parameters
validateParameters :: Int -> Int -> Int -> Either String ()
validateParameters rows cols mines
    | rows <= 0 || cols <= 0 = Left "Grid dimensions must be greater than 0."
    | mines < 0 = Left "Number of mines must be non-negative."
    | mines == 0 = Left "Number of mines must be greater than 0."
    | mines >= rows * cols = Left "Number of mines must be less than the total number of cells."
    | otherwise = Right ()

-- Ask the user for their first move
askFirstMove :: Grid -> Int -> Int -> Int -> IO (Either String Grid)
askFirstMove emptyGrid rows cols mines = do
    renderGrid emptyGrid
    putStrLn "\nEnter your first move (row,column):"
    input <- getLine
    case parseInput input of
        Just (False, firstRow, firstCol) ->
            if isOutOfBounds firstRow firstCol emptyGrid
                then do
                    putStrLn "Invalid move: Cell is out of bounds. Try again."
                    askFirstMove emptyGrid rows cols mines
                else do
                    grid <- generateRandomGrid rows cols mines (firstRow, firstCol)
                    let updatedGrid = revealCell firstRow firstCol grid
                    return $ Right updatedGrid
        _ -> do
            putStrLn "Invalid input format. Please use 'row,column'."
            askFirstMove emptyGrid rows cols mines

-- Centralized grid update function
processMove :: Bool -> Int -> Int -> Grid -> Either String Grid
processMove isFlag row col grid
    | isOutOfBounds row col grid = Left "Invalid move: Cell is out of bounds."
    | isFlag =
        if isRevealed cell
            then Left "Cannot flag a revealed cell."
            else Right $ flagCell row col grid
    | isRevealed cell = Left "Cell is already revealed. Choose another cell."
    | otherwise = Right $ revealCell row col grid
  where
    cell = grid V.! row V.! col

-- Game loop to play Minesweeper
gameLoop :: Grid -> Int -> Int -> IO ()
gameLoop grid rows cols = do
    renderGrid grid  -- Display the current state of the grid

    -- Check for a win before prompting for input
    if checkWin grid
        then putStrLn "\nCongratulations! You cleared the grid!"
        else do
            putStrLn "\nEnter your move (e.g., 'row,column' to reveal or 'F row,column' to flag):"
            input <- getLine

            case parseInput input of
                Just (isFlag, row, col) ->
                    case processMove isFlag row col grid of
                        Left err -> do
                            putStrLn err
                            gameLoop grid rows cols
                        Right updatedGrid ->
                            if checkWin updatedGrid
                                then putStrLn "\nCongratulations! You cleared the grid!"
                                else if isMine (updatedGrid V.! row V.! col) && not isFlag
                                    then do
                                        putStrLn "\nBOOM! You hit a mine. Game Over!"
                                        renderGrid updatedGrid
                                    else gameLoop updatedGrid rows cols
                Nothing -> do
                    putStrLn "Invalid input format. Please use 'row,column' or 'F row,column'."
                    gameLoop grid rows cols

-- Parse input of the form "r,c" or "F r,c"
parseInput :: String -> Maybe (Bool, Int, Int)
parseInput input =
    case trim input of
        ('F' : rest) -> parseCoords (trim rest) >>= \(row, col) -> Just (True, row, col)
        coords -> parseCoords coords >>= \(row, col) -> Just (False, row, col)
  where
    parseCoords str = case splitOn "," (trim str) of
        [rowStr, colStr] ->
            case (readMaybe rowStr, readMaybe colStr) of
                (Just row, Just col) -> Just (row, col)
                _ -> Nothing
        _ -> Nothing

-- Check if the player has won (all non-mine cells are revealed)
checkWin :: Grid -> Bool
checkWin = V.all (V.all (\cell -> isMine cell || isRevealed cell))