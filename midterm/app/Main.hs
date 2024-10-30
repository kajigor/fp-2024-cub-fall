module Main (main) where

import Directory

-- Implement a function that asks the user for the directory name and the sorting type, constructs the tree and then displays it accordingly.
run :: IO ()
run = do
  putStr "Enter the directory path: "
  hFlush stdout
  dirPath <- getLine
  putStr "Choose sorting type (default/alphabetical): "
  hFlush stdout
  sortType <- getLine
  tree <- buildTree dirPath
  let displayFunc = if sortType == "alphabetical"
                    then alphabetisedDisplayTree
                    else defaultDisplayTree
  putStrLn $ displayFunc tree

-- Use the main function to demostrate how run works. 
main :: IO ()
main = run
