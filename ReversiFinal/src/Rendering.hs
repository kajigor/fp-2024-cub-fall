module Rendering where

import Game

import Graphics.Gloss

import Data.Array

gameAsPicture :: Game -> Picture
boardGameOverPicture :: Board -> Maybe Player -> Picture
outcome :: Maybe Player -> Color

playerOneColor :: Color
playerTwoColor :: Color 
playerOneColor = green
playerTwoColor = red

playerCell :: Picture
playerCell = thickCircle radius 10.0
    where radius = min cellWidth cellHeight * 0.25

snapPictureToCell :: Picture -> (Int, Int) -> Picture
snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidth + cellWidth * 0.5
          y = fromIntegral row * cellHeight + cellHeight * 0.5

cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture =
    pictures
    $ map (snapPictureToCell cellPicture . fst)
    $ filter (\(_, e) -> e == cell)
    $ assocs board

outcome winner = case winner of
    Just Player1 -> playerOneColor
    Just Player2 -> playerTwoColor
    Nothing -> white

boardGrid :: Picture
boardGrid =
    pictures
    $ concatMap (\i -> [ line [ (i * cellWidth, 0.0)
                              , (i * cellWidth, fromIntegral screenHeight)
                              ]
                       , line [ (0.0,                      i * cellHeight)
                              , (fromIntegral screenWidth, i * cellHeight)
                              ]
                       ])
      [0.0 .. fromIntegral n]

playerOneBoard :: Board -> Picture
playerOneBoard board = cellsOfBoard board (Just Player1) playerCell

playerTwoBoard :: Board -> Picture      
playerTwoBoard board = cellsOfBoard board (Just Player2) playerCell

boardRunningPicture :: Board -> Picture 
boardRunningPicture board = pictures [color white boardGrid, color playerOneColor $ playerOneBoard board, color playerTwoColor $ playerTwoBoard board]

boardGameOverPicture board winner = color (outcome winner) $ boardAsPicture board

boardAsPicture :: Board -> Picture

boardAsPicture board =
    pictures [ boardGrid,
                playerOneBoard board,
                playerTwoBoard board
             ]


gameAsPicture game = translate (- fromIntegral screenWidth / 2) (- fromIntegral screenHeight / 2) $ frame
    where frame = case state game of Running -> boardRunningPicture (gameBoard game)
                                     GameOver winner -> boardGameOverPicture (gameBoard game) winner
