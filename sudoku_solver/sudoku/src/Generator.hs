module Generator (generateSolved, getEasySudoku, getMiddleSudoku, getHardSudoku) where

import Board
import Solver (findEmpty, isValid, solve)
import SampleBoards (emptyBoard)

import Data.Either

import System.Random (mkStdGen, randomIO)
import System.Random.Shuffle (shuffle')

getEasySudoku :: IO Board
getEasySudoku = do
    board <- generateSolved
    removeRandomly board 30

getMiddleSudoku :: IO Board
getMiddleSudoku = do
    board <- generateSolved
    removeRandomly board 40

getHardSudoku :: IO Board
getHardSudoku = do
    board <- generateSolved
    removeRandomly board 50

randomPermutation :: Int -> [a] -> [a]
randomPermutation seed xs = shuffle' xs (length xs) (mkStdGen seed)

generateSolved :: IO Board
generateSolved = fillRandomly emptyBoard

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

fillRandomly :: Board -> IO Board
fillRandomly board = case findEmpty board of
    Nothing -> return board
    Just (x, y) -> do
        seed1 <- randomIO
        let shuffledNumbers = randomPermutation seed1 [1..9]
        let validNumbers = filter (\val -> isValid board (x, y) val && isRight ( solve (setCell board (x, y) val))) shuffledNumbers
        seed2 <- randomIO
        let valid = randomPermutation seed2 validNumbers
        case headMay valid of
            Just v  -> fillRandomly $ setCell board (x, y) v
            Nothing -> return board

generateAllPairs :: [(Int, Int)]
generateAllPairs = [(r, c) | r <- [1..9], c <- [1..9]]

removeRandomly :: Board -> Int -> IO Board
removeRandomly board moves = do
    seed <- randomIO
    let shuffledPairs = randomPermutation seed generateAllPairs
    return $ foldr (\(x, y) b -> setCell b (x, y) 0) board (take moves shuffledPairs)
