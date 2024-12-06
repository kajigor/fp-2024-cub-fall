module SampleBoards (board1, board1Solved, solvedBoard, emptyBoard, invalidBoard) where

import Board
import qualified Data.Vector as V

listToBoard :: [[Int]] -> Board
listToBoard grid = V.fromList (map V.fromList grid)

board1 :: Board
board1 = listToBoard [
    [0, 3, 8, 0, 6, 0, 4, 1, 0],
    [0, 0, 0, 4, 7, 0, 0, 2, 0],
    [2, 0, 9, 0, 0, 0, 0, 6, 0],
    [0, 0, 5, 8, 0, 2, 3, 9, 0],
    [0, 0, 0, 0, 0, 4, 1, 0, 7],
    [9, 1, 0, 3, 5, 0, 2, 0, 0],
    [8, 9, 4, 0, 0, 5, 6, 7, 0],
    [5, 0, 7, 0, 4, 6, 8, 0, 0],
    [0, 0, 3, 0, 0, 0, 5, 0, 9]]

board1Solved :: Board
board1Solved = listToBoard [
    [7, 3, 8, 2, 6, 9, 4, 1, 5],
    [6, 5, 1, 4, 7, 3, 9, 2, 8],
    [2, 4, 9, 5, 8, 1, 7, 6, 3],
    [4, 7, 5, 8, 1, 2, 3, 9, 6],
    [3, 8, 2, 6, 9, 4, 1, 5, 7],
    [9, 1, 6, 3, 5, 7, 2, 8, 4],
    [8, 9, 4, 1, 3, 5, 6, 7, 2],
    [5, 2, 7, 9, 4, 6, 8, 3, 1],
    [1, 6, 3, 7, 2, 8, 5, 4, 9]]

invalidBoard :: Board
invalidBoard = listToBoard [
    [0, 8, 8, 0, 6, 0, 4, 1, 0],
    [0, 0, 0, 4, 7, 0, 0, 2, 0],
    [2, 0, 9, 0, 0, 0, 0, 6, 0],
    [0, 0, 5, 8, 0, 2, 3, 9, 0],
    [0, 0, 0, 0, 0, 4, 1, 0, 7],
    [9, 1, 0, 3, 5, 0, 2, 0, 0],
    [8, 9, 4, 0, 0, 5, 6, 7, 0],
    [5, 0, 7, 0, 4, 6, 8, 0, 0],
    [0, 0, 3, 0, 0, 0, 5, 0, 9]]

emptyBoard :: Board
emptyBoard = V.replicate 9 (V.replicate 9 0)

solvedBoard :: Board
solvedBoard = listToBoard [
    [5, 3, 4, 6, 7, 8, 9, 1, 2],
    [6, 7, 2, 1, 9, 5, 3, 4, 8],
    [1, 9, 8, 3, 4, 2, 5, 6, 7],
    [8, 5, 9, 7, 6, 1, 4, 2, 3],
    [4, 2, 6, 8, 5, 3, 7, 9, 1],
    [7, 1, 3, 9, 2, 4, 8, 5, 6],
    [9, 6, 1, 5, 3, 7, 2, 8, 4],
    [2, 8, 7, 4, 1, 9, 6, 3, 5],
    [3, 4, 5, 2, 8, 6, 1, 7, 9]]
