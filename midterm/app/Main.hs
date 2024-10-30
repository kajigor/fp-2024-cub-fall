module Main (main) where

import Directory (buildTree, defaultDisplayTree, alphabetisedDisplayTree)
import System.IO ()

-- Implement a function that asks the user for the directory name and the sorting type, constructs the tree and then displays it accordingly.
run :: IO () 
run = do
    putStrLn "Enter the directory path:"
    dirPath <- getLine
    putStrLn "Choose sorting type default display or alphabetised display:"
    sortingChoice <- getLine

    tree <- buildTree dirPath

    case sortingChoice of
        "1" -> putStrLn (defaultDisplayTree tree)
        "2" -> putStrLn (alphabetisedDisplayTree tree)
        _   -> putStrLn "Invalid choice. Please enter 1 or 2."

-- Use the main function to demostrate how run works. 
main :: IO ()
main = run 