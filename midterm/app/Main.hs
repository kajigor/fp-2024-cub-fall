module Main (main) where

import System.Directory
import System.IO
import Data.Char (toLower)
import qualified Directory as D

run :: IO ()
run = do
    putStr "Enter directory path: "
    hFlush stdout
    path <- getLine
    putStr "Sort alphabetically? (y/n): "
    hFlush stdout
    sortType <- getLine
    tree <- D.buildTree path
    putStrLn $ if toLowerCase sortType == "y"
        then D.alphabetDisplayTree tree
        else D.defaultDisplayTree tree
  where
    toLowerCase = map toLower

main :: IO ()
main = do
    run