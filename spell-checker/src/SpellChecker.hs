module SpellChecker (run) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Dictionary as Dict
import Control.Monad (when)
import ErrorHandler (safeReadFile, safeWriteFile)

-- Main run function
run :: IO ()
run = do
    putStrLn "Enter the path to the dictionary file:"
    dictPath <- getLine
    dict <- Dict.loadDictionary dictPath
    putStrLn "Enter the path to the text file to check:"
    textPath <- getLine
    textResult <- safeReadFile textPath
    case textResult of
        Left err -> putStrLn ("Error reading file: " <> err)
        Right content -> do
            let wordsToCheck = Dict.splitWords content
            mapM_ (localCheckWord dict) wordsToCheck
            addCustomWordsLoop dict (T.pack dictPath)

-- Local function to check a single word
localCheckWord :: Dict.Dictionary -> T.Text -> IO ()
localCheckWord dict word = do
    let clean = Dict.cleanWord word
    if Dict.checkWord dict clean
        then return ()
        else do
            putStrLn $ T.unpack word <> " is incorrect."
            putStrLn "Suggestions:"
            Dict.printSuggestions (Dict.suggestCorrections dict clean)

-- Loop to add custom words
addCustomWordsLoop :: Dict.Dictionary -> T.Text -> IO ()
addCustomWordsLoop dict dictPath = do
    putStrLn "Would you like to add a custom word to the dictionary? (yes/no)"
    response <- getLine
    if response == "yes"
        then do
            putStrLn "Enter the custom word:"
            customWord <- getLine
            let updatedDict = Dict.addCustomWord (T.pack customWord) dict
            Dict.saveCustomWords dictPath updatedDict
            putStrLn "Custom word added!"
            addCustomWordsLoop updatedDict dictPath
        else putStrLn "Goodbye!"
