module Main where

import System.IO
import System.Directory (getCurrentDirectory, doesFileExist)
import Lib

main :: IO ()
main = do
    putStrLn "Haskell Text File Analyzer"
    putStrLn "=========================="
    currentDir <- getCurrentDirectory
    putStrLn $ "Current working directory: " ++ currentDir
    putStr "Please provide the path to your text file: "
    hFlush stdout
    filePath <- getLine
    checkFile filePath