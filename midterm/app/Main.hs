module Main (main) where

import Directory

-- Implement a function that asks the user for the directory name and the sorting type, constructs the tree and then displays it accordingly.
run :: IO () 
run = do
  putStrLn "Enter the directory path:"
  dirPath <- getLine
  putStrLn "Enter sorting type (default/alphabetical):"
  sortingType <- getLine
  tree <- buildTree dirPath
  let displayFunc = case sortingType of
          "alphabetical" -> alphabetisedDisplayTree
          _              -> defaultDisplayTree
  putStrLn (displayFunc tree)

-- Use the main function to demostrate how run works. 
main :: IO ()
main = run
