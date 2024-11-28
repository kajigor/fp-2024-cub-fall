{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Reader
import Text.Read (readMaybe)

main :: IO ()
main = do
    putStrLn "Input the path of the file for a text file analysis"
    path_file <- getLine
    fileExists <- fileCheck path_file
    if fileExists
        then do
            content <- processFileMaybe path_file
            case content of
                Just fileContent -> do
                    numWords <- wordCounter fileContent
                    putStrLn $ "Number of words: " ++ show numWords
                    numLines <- lineCounter fileContent
                    putStrLn $ "Number of lines: " ++ show numLines
                    numChar <- characterCounter fileContent
                    putStrLn $ "Number of characters: " ++ show numChar
                    frqtWord <- frequentWord fileContent
                    putStrLn $ "Frequent word: " ++ show frqtWord
                    number <- checkNum
                    putStrLn "N-grams:"
                    ngram fileContent number
                    putStrLn "Cloud representation:"
                    couldRepresentation fileContent
                Nothing -> putStrLn "File is empty"
        else putStrLn "File doesn't exist"

checkNum :: IO Int
checkNum = do
    putStrLn "Enter a number for the ngram analysis:"
    number <- getLine
    let maybeNum = readMaybe @Int number
    case maybeNum of
        Just n | n > 0 -> return n
        _ -> do
            putStrLn "Invalid number"
            checkNum
    