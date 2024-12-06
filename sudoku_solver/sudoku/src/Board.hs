module Board (printBoard, Board, setCell) where

import qualified Data.Vector as V
import qualified Control.Monad

lineSeparator :: String
lineSeparator =  "+---------+---------+---------+"

lineSeparatorTop :: String
lineSeparatorTop =  "#---------+---------+---------#"

lineSeparatorBottom :: String
lineSeparatorBottom =  "#---------+---------+---------#"

type Board = V.Vector (V.Vector Int)

getCell :: Board -> (Int, Int) -> Int
getCell board (r, c) = (board V.! (r - 1)) V.! (c - 1)

setCell :: Board -> (Int, Int) -> Int -> Board
setCell board (r, c) value = board V.// [(r - 1, (board V.! (r - 1)) V.// [(c - 1, value)])]

printBoard :: Board -> IO ()
printBoard board = do
  putStrLn lineSeparatorTop
  mapM_ printSudokuRow [1..9]
  putStrLn lineSeparatorBottom
  where
    printSudokuRow r = do
      mapM_ (printCell r) [1..9]
      Control.Monad.when (r `mod` 3 == 0 && r < 9) $ putStrLn lineSeparator

    printCell r c = do
      let x = getCell board (r, c)
      let border = if c `mod` 3 == 0 then "| " else " "
      if c == 1
        then putStr "| "
        else putStr ""
      if x == 0
        then putStr ". "
        else putStr (show x ++ " ")
      putStr border
      if c == 9
        then putStr "\n"
        else putStr ""
