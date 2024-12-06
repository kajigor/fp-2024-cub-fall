module Main (main) where

import Lib
import Board (printBoard, Board, setCell)
import SampleBoards (board1, solvedBoard, invalidBoard)
import Solver (solve, findEmpty, isValid)
import Generator (generateSolved, getEasySudoku, getMiddleSudoku, getHardSudoku)
import Text.Read (readMaybe)

main :: IO ()
main = start

start :: IO ()
start = do
    putStrLn "Welcome to the Sudoku solver!"
    putStrLn "Choose an option: "
    putStrLn "1. Play sudoku"
    putStrLn "2. Generate and solve sudoku"
    putStrLn "3. Exit"
    getOptionStart

getOptionStart :: IO ()
getOptionStart = do
    option <- getLine
    case option of
        "1" -> difficultyChoice getOptionPlayMenu
        "2" -> difficultyChoice genAndSolveMenu
        "3" -> putStrLn "Goodbye!"
        _   -> do
            putStrLn "Invalid option. Please try again."
            getOptionStart

difficultyChoice :: IO () -> IO ()
difficultyChoice f = do
    putStrLn "Choose difficulty: "
    putStrLn "1. Easy"
    putStrLn "2. Medium"
    putStrLn "3. Hard"
    f

getOptionPlayMenu :: IO ()
getOptionPlayMenu = do
    option <- getLine
    case option of
        "1" -> do
            putStrLn "This one's so easy, even a child could solve it. Let's see if you can keep up."
            play getEasySudoku
        "2" -> do
            putStrLn "Ah, a medium-level puzzle. Lets see if you can handle something thats not completely brain-dead."
            play getMiddleSudoku
        "3" -> do
            putStrLn "Good luck with this one. If you manage to finish it, maybe you are actually smarter than you look."
            play getHardSudoku
        _   -> do
            putStrLn "Invalid option. Please try again."
            getOptionPlayMenu


play :: IO Board -> IO ()
play board = do
    putStrLn "Current board:"
    bboard <- board
    printBoard bboard
    case findEmpty bboard of
        Nothing -> putStrLn "Congrats! You've solved it"
        _ -> do
            putStrLn "Enter your turn in form of x y val, where x and y are coordinates of the cell in 1-indexation, and val is the value you are setting"
            input <- getLine
            let nums = map readMaybe (words input) :: [Maybe Int]
            case nums of
                [Just x, Just y, Just val] -> do
                    let next = setCell bboard (y, x) val
                    case (isValid bboard (y, x) val, solve next) of
                        (True, Right res) -> play $ return next
                        _ -> do
                            putStrLn "Invalid turn. Please try again"
                            play (return bboard)
                _ -> do
                    putStrLn "Invalid input. Please enter exactly three integers."
                    play (return bboard)
    main

genAndSolveMenu :: IO ()
genAndSolveMenu = do
    option <- getLine
    case option of
        "1" -> do
            putStrLn "Sure, here is you randomly generated easy sudoku"
            genAndSolve getEasySudoku
        "2" -> do
            putStrLn "Sure, here is you randomly generated medium sudoku"
            genAndSolve getMiddleSudoku
        "3" -> do
            putStrLn "Sure, here is you randomly generated hard sudoku"
            genAndSolve getHardSudoku
        _   -> do
            putStrLn "Invalid option. Please try again."
            getOptionPlayMenu

genAndSolve :: IO Board -> IO ()
genAndSolve board = do 
    putStrLn "Initial board:"
    bboard <- board
    printBoard bboard
    case solve bboard of
        Right solution -> do
            putStrLn "And here is the solution!"
            printBoard solution
        Left err -> do
            putStrLn "This sudoku has no solutions"
    main