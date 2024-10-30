module Main (main) where

import Directory
import System.IO (hFlush, stdout)

run :: IO ()
run = do
    putStr "Enter the directory path: "
    hFlush stdout
    directoryPath <- getLine
    putStr "Enter sorting type (default/alphabetical): "
    hFlush stdout
    sortingType <- getLine
    tree <- buildTree directoryPath
    case sortingType of
        "default" -> putStrLn (defaultDisplayTree tree)
        "alphabetical" -> putStrLn (alphabetisedDisplayTree tree)
        _ -> putStrLn "Invalid sorting type. Please use 'default' or 'alphabetical'."

main :: IO ()
main = run