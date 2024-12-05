module Game where

import Data.Array

data Player = Player1 | Player2 deriving (Show, Eq)

data State = Running | GameOver (Maybe Player) deriving (Show, Eq) 

type Cell = Maybe Player

type Board = Array (Int, Int) Cell


data Game = Game
  { gameBoard :: Board
  , player :: Player
  , state :: State
  , screenWidth ::Int
  , screenHeight :: Int
  , cellWidth :: Float
  , cellHeight :: Float
  , n :: Int
  } deriving (Show, Eq)

oppositePlayer :: Game -> Player
oppositePlayer game = case player game of
    Player1 -> Player2
    Player2 -> Player1
