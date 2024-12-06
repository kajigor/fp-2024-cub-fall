module ConsoleUI where

import GameLogic
import Board

import Data.Char (chr, ord, isSpace)
import Data.List (dropWhileEnd)

import Text.Read (readMaybe)

renderBoard :: Board -> String
renderBoard board =
  dropWhileEnd isSpace ("  " ++ concatMap (: " ") (take (length board) ['A' ..])) ++ "\n"
  ++ unlines (map renderRow (zip [1..] board))
  where
    renderRow (rowNum, row) = dropWhileEnd isSpace $ show rowNum ++ " " ++ concatMap renderCell row
    renderCell Nothing = ". "
    renderCell (Just Black) = "B "
    renderCell (Just White) = "W "

parseInput :: String -> Maybe (Int, Int)
parseInput input =
  if length input < 2 then Nothing
  else
    let (rowPart, colPart) = splitAt (length input - 1) input
        row = readMaybe rowPart :: Maybe Int
        col = toColIndex (head colPart)
    in case (row, col) of
        (Just r, Just c) -> Just (r - 1, c)
        _ -> Nothing
  where
    toColIndex c
      | c >= 'A' && c <= 'H' = Just (ord c - ord 'A')
      | c >= 'a' && c <= 'h' = Just (ord c - ord 'a')
      | otherwise = Nothing

startGame :: IO ()
startGame = do
  putStrLn "Welcome to Reversi!"
  putStrLn "Enter the size of the board (minimum size is 4, default is 8):"
  sizeInput <- getLine
  let size = readMaybe sizeInput :: Maybe Int
  case size of
    Just s | s >= 4 -> gameLoop (initGame s)
    _ -> do
      putStrLn "Invalid board size. Please enter a valid number greater than or equal to 4."
      startGame

gameLoop :: GameState -> IO ()
gameLoop state@(GameState board player _) = do
  putStrLn $ renderBoard board
  putStrLn $ "Player " ++ show player ++ "'s turn."
  putStrLn "Enter your move as row-column (e.g., 4C):"
  input <- getLine
  case parseInput input of
    Nothing -> do
      putStrLn "Invalid input. Please use the format row-column (e.g., 4C)."
      gameLoop state
    Just move -> case applyMove state move of
      Left err -> do
        handleMoveError err
        gameLoop state
      Right newState -> gameLoop newState

handleMoveError :: MoveError -> IO ()
handleMoveError err = case err of
  OutOfBounds -> putStrLn "Invalid move: The position is out of bounds."
  PositionOccupied -> putStrLn "Invalid move: The position is already occupied."
  CannotFlip -> putStrLn "Invalid move: No discs can be flipped."
