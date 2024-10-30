module Main (main) where

import Directory

-- Implement a function that asks the user for the directory name and the sorting type, constructs the tree and then displays it accordingly.
run :: IO () 
run = do
    putStrLn "Directory to display:"
    dirPath <- getLine

    putStrLn "Choose sorting type (default or alphabetised): "
    sortingType <- getLine

    case sortingType of
        "default" -> do
            tree <- buildTree dirPath
            putStrLn $ defaultDisplayTree tree
        "alphabetical" -> do
            tree <- buildTree dirPath
            putStrLn $ alphabetisedDisplayTree tree
        _ -> do
            putStrLn "Invalid sorting type. Exiting."

-- Use the main function to demostrate how run works. 
main :: IO ()
main = run