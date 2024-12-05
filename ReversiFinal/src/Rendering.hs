module Rendering where

import Game
import Graphics.Gloss
import Data.Array

-- Renders the entire game as a Picture
gameAsPicture :: Game -> Picture
gameAsPicture game =
    translate (- fromIntegral (screenWidth game) / 2) (- fromIntegral (screenHeight game) / 2) frame
  where
    frame = case state game of
        Running -> boardRunningPicture game
        GameOver winner -> boardGameOverPicture game winner

-- Produces the board when the game is over, coloring it based on the winner
boardGameOverPicture :: Game -> Maybe Player -> Picture
boardGameOverPicture game winner = color (outcome winner) $ boardAsPicture game

-- Returns the color outcome based on the winner
outcome :: Maybe Player -> Color
outcome winner = case winner of
    Just Player1 -> playerOneColor
    Just Player2 -> playerTwoColor
    Nothing -> white

-- Colors for Player1 and Player2
playerOneColor :: Color
playerTwoColor :: Color
playerOneColor = green
playerTwoColor = red

-- Renders a player cell (piece) as a Picture
playerCell :: Game -> Picture
playerCell game = thickCircle radius 10.0
  where
    radius = min (cellWidth game) (cellHeight game) * 0.25

-- Snap a Picture to a specific cell on the board
snapPictureToCell :: Game -> Picture -> (Int, Int) -> Picture
snapPictureToCell game picture (row, column) = translate x y picture
  where
    x = fromIntegral column * cellWidth game + cellWidth game * 0.5
    y = fromIntegral row * cellHeight game + cellHeight game * 0.5

-- Renders all cells of a specific type
cellsOfBoard :: Game -> Cell -> Picture -> Picture
cellsOfBoard game cell cellPicture =
    pictures
    $ map (snapPictureToCell game cellPicture . fst)
    $ filter (\(_, e) -> e == cell)
    $ assocs (gameBoard game)

-- Produces a grid for the board
boardGrid :: Game -> Picture
boardGrid game =
    pictures
    $ concatMap (\i -> [ line [(i * cellWidth game, 0.0), (i * cellWidth game, fromIntegral (screenHeight game))]
                       , line [(0.0, i * cellHeight game), (fromIntegral (screenWidth game), i * cellHeight game)]
                       ])
      [0.0 .. fromIntegral (n game)]

-- Renders all Player1 cells
playerOneBoard :: Game -> Picture
playerOneBoard game = cellsOfBoard game (Just Player1) (playerCell game)

-- Renders all Player2 cells
playerTwoBoard :: Game -> Picture
playerTwoBoard game = cellsOfBoard game (Just Player2) (playerCell game)

-- Produces the board when the game is running
boardRunningPicture :: Game -> Picture
boardRunningPicture game =
    pictures
    [ color white (boardGrid game)
    , color playerOneColor (playerOneBoard game)
    , color playerTwoColor (playerTwoBoard game)
    ]

-- Produces the board as a Picture
boardAsPicture :: Game -> Picture
boardAsPicture game =
    pictures [ boardGrid game
             , playerOneBoard game
             , playerTwoBoard game
             ]
