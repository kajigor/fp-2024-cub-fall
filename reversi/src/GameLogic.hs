module GameLogic where

import Board

import Data.Maybe (isNothing, isJust)

data MoveError
  = OutOfBounds
  | PositionOccupied
  | CannotFlip
  deriving (Eq, Show)

opponent :: Disc -> Disc
opponent Black = White
opponent White = Black

directions :: [(Int, Int)]
directions = [(dx, dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx, dy) /= (0, 0)]

canFlipInDirection :: Board -> Disc -> (Int, Int) -> (Int, Int) -> Bool
canFlipInDirection board player (x, y) (dx, dy) = go (x + dx, y + dy) False
  where
    go (nx, ny) hasOpponent
      | nx < 0 || ny < 0 || nx >= length board || ny >= length board = False
      | isNothing (board !! nx !! ny) = False
      | board !! nx !! ny == Just player = hasOpponent
      | board !! nx !! ny == Just (opponent player) = go (nx + dx, ny + dy) True
      | otherwise = False

isValidMove :: GameState -> (Int, Int) -> Either MoveError ()
isValidMove (GameState board player _) (x, y)
  | not inBounds = Left OutOfBounds
  | isJust (board !! x !! y) = Left PositionOccupied
  | not canFlip = Left CannotFlip
  | otherwise = Right ()
  where
    size = length board
    inBounds = x >= 0 && y >= 0 && x < size && y < size
    canFlip = any (canFlipInDirection board player (x, y)) directions

getDisksToFlip :: Disc -> (Int, Int) -> (Int, Int) -> Board -> [(Int, Int)]
getDisksToFlip player (x, y) (dx, dy) board
  | x < 0 || y < 0 || x >= length board || y >= length board = []
  | isNothing (board !! x !! y) = []
  | board !! x !! y == Just player = []
  | otherwise =
      let nextToFlip = getDisksToFlip player (x + dx, y + dy) (dx, dy) board
      in if null nextToFlip && isNothing (board !! (x + dx) !! (y + dy))
         then []
         else (x, y) : nextToFlip


flipDiscs :: Disc -> (Int, Int) -> Board -> (Int, Int) -> Board
flipDiscs player (x, y) board (dx, dy) =
  let toFlip = getDisksToFlip player (x + dx, y + dy) (dx, dy) board
  in if not (null toFlip)
     then foldl (\b (fx, fy) -> updateBoard b (fx, fy) (Just player)) board toFlip
     else board

applyMove :: GameState -> (Int, Int) -> Either MoveError GameState
applyMove state@(GameState board player history) pos =
  case isValidMove state pos of
    Left err -> Left err
    Right _  -> Right $ GameState newBoard nextPlayer (board : history)
  where
    nextPlayer = if player == Black then White else Black
    newBoard = foldl (flipDiscs player pos) (updateBoard board pos (Just player)) directions
