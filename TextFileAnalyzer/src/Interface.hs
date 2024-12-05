module Interface (mainMenu) where

import System.IO (hFlush, stdout)
import Statistics (computeStatistics, displayStatistics)
import FrequentWords (findMostFrequentWords, displayFrequentWords)
import NGramAnalysis (generateNGrams, displayNGrams)
import WordCloud (generateWordCloud, displayWordCloud)

mainMenu :: String -> IO ()
mainMenu content = do
    putStrLn "\nText File Analyzer Menu:"
    putStrLn "1. View Basic Statistics"
    putStrLn "2. Find Most Frequent Words"
    putStrLn "3. Perform N-Gram Analysis"
    putStrLn "4. Generate Word Cloud"
    putStrLn "5. Exit"
    putStr "Enter your choice: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> do
            let stats = computeStatistics content
            displayStatistics stats
            mainMenu content
        "2" -> do
            putStr "Enter the number of top frequent words to display: "
            hFlush stdout
            nStr <- getLine
            case reads nStr :: [(Int, String)] of
                [(n, "")] -> do
                    let frequentWords = findMostFrequentWords n content
                    displayFrequentWords frequentWords
                _ -> putStrLn "Invalid input for number of words."
            mainMenu content
        "3" -> do
            putStr "Enter the value of n for n-gram analysis: "
            hFlush stdout
            nStr <- getLine
            case reads nStr :: [(Int, String)] of
                [(n, "")] -> do
                    let ngrams = generateNGrams n content
                    displayNGrams ngrams
                _ -> putStrLn "Invalid input for n-gram value."
            mainMenu content
        "4" -> do
            let wordCloud = generateWordCloud content
            displayWordCloud wordCloud
            mainMenu content
        "5" -> putStrLn "Exiting..."
        _   -> do
            putStrLn "Invalid choice. Please try again."
            mainMenu content
