{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Solver (solve, findEmpty, isValid) where

import Board
import qualified Data.Vector as V
import qualified Data.Maybe

isValid :: Board -> (Int, Int) -> Int -> Bool
isValid board (r, c) value = notInRow && notInCol && notInSubGrid
  where
    notInRow = all (\x -> (board V.! (r - 1)) V.! (x - 1) /= value) [1..9]
    notInCol = all (\x -> (board V.! (x - 1)) V.! (c - 1) /= value) [1..9]
    subGridRow = ((r - 1) `div` 3) * 3
    subGridCol = ((c - 1) `div` 3) * 3
    notInSubGrid = all (\(x, y) -> (board V.! x) V.! y /= value)
                      [(x, y) | x <- [subGridRow..subGridRow+2], y <- [subGridCol..subGridCol+2]]

findEmpty :: Board -> Maybe (Int, Int)
findEmpty board = case filter (\(r, c) -> (board V.! (r - 1)) V.! (c - 1) == 0) [(r, c) | r <- [1..9], c <- [1..9]] of
    []          -> Nothing
    (x:_)       -> Just x

solve :: Board -> Either String Board
solve board = case solve1 board of
    Just res -> Right res
    Nothing -> Left "No solutions"

solve1 :: Board -> Maybe Board
solve1 board =
    case findEmpty board of
        Nothing     -> Just board
        Just pos -> tryNumbers board pos [1..9]

tryNumbers :: Board -> (Int, Int) -> [Int] -> Maybe Board
tryNumbers board pos [] = Nothing
tryNumbers board pos (n:ns)
    | isValid board pos n =
        case solve1 (setCell board pos n) of
            Just solvedBoard -> Just solvedBoard
            Nothing          -> tryNumbers board pos ns
    | otherwise = tryNumbers board pos ns
