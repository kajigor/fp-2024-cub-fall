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
  } deriving (Show, Eq)

n :: Int

n = 8

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

oppositePlayer :: Game -> Player
oppositePlayer game = case player game of
    Player1 -> Player2
    Player2 -> Player1

initialGame :: Game
initialGame = Game { gameBoard = (array indexRange $ zip (range indexRange) (repeat Nothing)) // [((3, 3), Just Player2), ((4, 4), Just Player2), ((3, 4), Just Player1), ((4, 3), Just Player1)]   
                   , player = Player1
                   , state = Running 
                   }
    where indexRange = ((0, 0), (n - 1, n - 1))