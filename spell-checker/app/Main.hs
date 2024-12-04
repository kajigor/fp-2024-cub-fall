module Main where

import Lib
import System.IO (hFlush, stdout)

mainLoop :: Trie -> IO ()
mainLoop trie = do
    putStrLn "Spell Checker Menu"
    putStrLn "1. Check text"
    putStrLn "2. Add a custom word to the dictionary"
    putStrLn "3. Quit"
    putStrLn "Enter your choice: "
    hFlush stdout
    choice <- getLine

    case choice of
        "1" -> do
            checkText trie
            mainLoop trie
        "2" -> do
            putStrLn "Enter the word to add to the dictionary:"
            word <- getLine
            newTrie <- addWord trie word
            putStrLn $ "Added word: " ++ word
            mainLoop newTrie
        "3" -> putStrLn "Exiting Program."
        _ -> do 
            putStrLn "Invalid choice. Please try again."
            mainLoop trie

main :: IO ()
main = do
    trie <- loadDictionary emptyTrie
    mainLoop trie
