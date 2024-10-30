module Main (main) where

import Directory
import System.IO (stdout)

-- Implement a function that asks the user for the directory name and the sorting type, constructs the tree and then displays it accordingly.
run :: IO () 
run = do
    putStrLn "input directory path:"
    dirPath <- getLine

    putStrLn "input 1 for default, 2 for alphabetized"
    displayChoice <- getLine

    tree <- buildTree dirPath

    case displayChoice of
        "1" -> putStrLn $ defaultDisplayTree tree
        "2" -> putStrLn $ alphabetisedDisplayTree tree
        _   -> putStrLn "only 1 or 2."

-- Use the main function to demostrate how run works. 
main :: IO ()
main = run 