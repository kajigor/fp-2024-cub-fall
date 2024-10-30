module Main (main) where

import Directory


-- Implement a function that asks the user for the directory name and the sorting type, constructs the tree and then displays it accordingly.
run :: IO () 
run = do
    putStr "Enter directory path: "
    hFlush stdout
    dirPath <- getLine
    putStrLn "Sort type (1 - alphabet, 0 - default)"
    hFlush stdout
    sortType <- getLine
    tree <- buildTree dirPath
    putStrLn $ defaultDisplayTree tree

-- Use the main function to demostrate how run works. 
main :: IO ()
main = run