module Main (main) where

import Directory
import System.IO (hFlush, stdout)

-- Implement a function that asks the user for the directory name and the sorting type, constructs the tree and then displays it accordingly.
run :: IO () 
run = do
  putStrLn "Enter the directory path:"
  putStr "> "
  hFlush stdout
  dirPath <- getLine

  putStrLn "Display options:"
  putStrLn "d: default"
  putStrLn "a: alphabetical"
  putStr "> "
  hFlush stdout
  choice <- getLine

  tree <- buildTree dirPath
  let output = case choice of
                 "d" -> defaultDisplayTree tree
                 "a" -> alphabetisedDisplayTree tree
                 _   -> "Invalid choice. Please enter 'a' or 'a'."
  
  putStrLn "\nDirectory Structure:"
  putStrLn output

-- Use the main function to demostrate how run works. 
main :: IO ()
main = run