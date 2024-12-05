module Main (main) where

import Graphics.Gloss
import Game
import Logic
import Rendering
window :: Display
window = InWindow "Reversi Game" (640, 480) (10, 10)

backgroundColor :: Color
backgroundColor = black

main :: IO ()
-- main = display window backgroundColor (color white boardGrid)
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (\_ -> id)
