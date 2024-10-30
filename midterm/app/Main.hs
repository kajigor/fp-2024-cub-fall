module Main (main) where

import Directory

-- Implement a function that asks the user for the directory name and the sorting type, constructs the tree and then displays it accordingly.
run :: IO () 
run = do
    putStrLn "Enter the directory path: "
    dir <- getLine
    putStrLn ("Which sorting do you prefer: default/alphabetised: ")
    sorting <- getLine
    tree <- buildTree dir

    case sorting of
        "default" -> putStr $ defaultDisplayTree tree
        "alphabetised" -> putStr $ alphabetisedDisplayTree tree
        _ -> putStrLn "Invalid sorting. Please enter exactly 'default' or 'alphabetised'"

-- Use the main function to demostrate how run works. 
main :: IO ()
main = undefined 