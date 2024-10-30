module Main (main) where

import Directory
import System.IO (hFlush, stdout)
import Control.Monad (unless)

run :: IO () 
run = do
    putStr "Please enter the directory path (or leave it empty for current directory <3): "
    hFlush stdout
    dirPath <- getLine
    let directory = if null dirPath then "." else dirPath

    putStr "Please enter the sorting type: "
    hFlush stdout
    sortType <- getLine

    tree <- buildTree directory

    case sortType of
        "default"      -> putStrLn (defaultDisplayTree tree)
        "alphabetised" -> putStrLn (alphabetisedDisplayTree tree)
        _              -> putStrLn "Invalid sorting type! Please enter 'default' or 'alphabetised'."


main :: IO ()
main = run 