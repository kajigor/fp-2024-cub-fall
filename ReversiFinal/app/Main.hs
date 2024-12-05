module Main (main) where

import Graphics.Gloss
import Game
import Logic
import Rendering
import Data.Array

backgroundColor :: Color
backgroundColor = black
validateInput :: String -> Maybe Int
validateInput s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing  

main :: IO ()
-- main = display window backgroundColor (color white boardGrid)
main = do
  putStrLn "Enter the size of the Board:"
  input1 <- getLine
  putStrLn "Enter the screen Width:"
  input2 <- getLine
  putStrLn "Enter the screen Height:"
  input3 <- getLine

  case (validateInput input1, validateInput input2, validateInput input3) of
    (Just n1, Just n2, Just n3)-> do
      if n1 `mod` 2 /= 0 then do
        putStrLn "Error: The size of the board must be an even number."
        return ()
      else do
        putStrLn $ "You entered valid integers: " ++ show n1 ++ ", " ++ show n2 ++ ", " ++ show n3
        let indexRange = ((0, 0), (n1 - 1, n1 - 1))
        let n1Half = n1 `div` 2
        let initialGame :: Game
            initialGame = Game { 
                            n = n1
                        , gameBoard = (array indexRange $ zip (range indexRange) (repeat Nothing)) // [((n1Half - 1, n1Half - 1), Just Player2), ((n1Half, n1Half), Just Player2), ((n1Half - 1, n1Half), Just Player1), ((n1Half, n1Half - 1), Just Player1)]
                        , player = Player1
                        , state = Running 
                        , screenWidth = n2
                        , screenHeight = n3
                        , cellWidth = fromIntegral n2 / fromIntegral n1
                        , cellHeight = fromIntegral n3 / fromIntegral n1
                        }
        let window :: Display
            window = InWindow "Reversi" (n2, n3) (10, 10)
        play window backgroundColor 30 initialGame gameAsPicture transformGame (\_ -> id)
    _ -> do
      putStrLn "Error: Invalid input detected."
