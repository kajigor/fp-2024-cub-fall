module Lib where

import System.IO
import System.Directory (getCurrentDirectory, doesFileExist)
import Control.Exception (catch, IOException)
import Data.Char
import Data.List
import Text.Read (readMaybe)
import Control.Monad
import System.Random
import qualified Data.Map as Map

--Main menu for navigation and commands
menu :: String -> IO ()
menu content = do
    putStrLn "Main Menu"
    putStrLn "========="
    putStrLn "Choose an option to be performed on your loaded file"
    putStrLn "1. Compute file statistics (word count, line count, character count)"
    putStrLn "2. Identify most frequent word"
    putStrLn "3. Implement n-gram analysis for sequences of words"
    putStrLn "4. Generate a textual word cloud representation"
    putStrLn "0. Close the program"
    putStr "Enter your choice: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> do
            putStrLn "File Statistics"
            putStrLn "==============="
            putStrLn $ "Word count: " ++ show (wordCount content)
            putStrLn $ "Line count: " ++ show (linesCount content)
            putStrLn $ "Character count: " ++ show (charCount content)
            goBack content
        "2" -> do
            commonWords content
            goBack content
        "3" -> do
            askForNGram content
            goBack content
        "4" -> do
            createCloud content
            goBack content
        "0" -> do
            putStrLn "Closing the program..."
            return ()
        _ -> do
            putStrLn "Invalid input! Please enter a number between 0 and 4!"
            menu content
    
goBack :: String -> IO ()
goBack content = do
    putStr "Type \"back\" when you want to return to the main menu: "
    hFlush stdout
    choice <- getLine
    case choice of
        "back" -> menu content
        _ -> do
            putStrLn "Invalid choice. Type \"back\" when you want to return to the main menu:"
            goBack content

wordCount :: String -> Int
wordCount content = length . words $ content

linesCount :: String -> Int
linesCount content = length . lines $ content

charCount :: String -> Int
charCount content = length content

--Helper function that generates a list of word-frequency tuples - used for testing
commonWordsHelper :: String -> [(String, Int)]
commonWordsHelper content =
    let wordsList = map (filter isValidChar) (words (map toLower content))
        frequencyMap = foldl' (\m word -> Map.insertWith (+) word 1 m) Map.empty wordsList
    in Map.toList frequencyMap

commonWords :: String -> IO()
commonWords content = do
    let frequency = commonWordsHelper content
        top = take 10 (sortOn (negate . snd) frequency)
    putStrLn "Here is the list of the ten most frequent words in your file: "
    mapM_ (\(w, fr) -> putStrLn("( " ++ show w ++ " )" ++ " appears " ++ show fr ++ " times")) top
    askForFullList frequency

--Generates a list of n-grams
generateNGrams :: Int -> [String] -> [String]
generateNGrams n wordsList
    | n <= 0 = []
    | otherwise = [unwords (take n (drop i wordsList)) | i <- [0..length wordsList - n]]

--Helper function which generates a list of n-gram-frequency tuples - used for testing
nGramHelper :: String -> Int -> [(String, Int)]
nGramHelper content n = 
    let wordsList = map (filter isValidChar) (words (map toLower content))
        grams = generateNGrams n wordsList
        frequencyMap = foldl' (\m word -> Map.insertWith (+) word 1 m) Map.empty grams
    in Map.toList frequencyMap
    
nGramAnalysis :: String -> Int -> IO()
nGramAnalysis content n = do
    let frequency = nGramHelper content n
        top = take 10 (sortOn (negate . snd) frequency)
    putStrLn ("Here is the list of the ten most frequent " ++ show n ++ "-grams in your file: ")
    mapM_ (\(w, fr) -> putStrLn("( " ++ show w ++ " )" ++ " appears " ++ show fr ++ " times")) top      
    askForFullList frequency  

askForNGram :: String -> IO()
askForNGram content = do
    n <- getValidInt "For what n value do you want to perform n-gram analysis: "
    nGramAnalysis content n
    getValidChoice content

--Styles the words in the cloud based on their importance levels
styleCloud :: (String, Int) -> String
styleCloud (word, importance) = 
    case importance of
        1 -> map toLower word
        2 -> capitalize word
        3 -> map toUpper word

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

--Transforms the content of the text file into a list of tuples that contain words and an assigned importance level based on frequency
transform :: String -> [(String, Int)]
transform content =
    let wordsList = map (filter isValidChar) (words (map toLower content))
        frequencyMap = foldl' (\m word -> Map.insertWith (+) word 1 m) Map.empty wordsList
        frequency = Map.toList frequencyMap
        sortedFreq = sortOn (negate . snd) frequency
        total = length sortedFreq
    in if total >= 3
        then
            let (high, mediumHigh) = splitAt (total `div` 3) sortedFreq
                (medium, low) = splitAt (total `div` 3) mediumHigh
                (adjustedHigh, newMedium) = adjustLists high medium
                (finalMedium, adjustedLow) = adjustLists newMedium low
            in introduceLevel 3 adjustedHigh ++ introduceLevel 2 finalMedium ++ introduceLevel 1 adjustedLow
        else if total == 2
            then
                if snd (head sortedFreq) > snd (last sortedFreq)
                    then introduceLevel 3 [(fst (head sortedFreq), snd (head sortedFreq))] ++ introduceLevel 1 [(fst (last sortedFreq), snd (last sortedFreq))]
                    else if snd (head sortedFreq) < snd (last sortedFreq)
                        then introduceLevel 1 [(fst (head sortedFreq), snd (head sortedFreq))] ++ introduceLevel 3 [(fst (last sortedFreq), snd (last sortedFreq))]
                        else introduceLevel 1 [(fst (head sortedFreq), snd (head sortedFreq))] ++ introduceLevel 1 [(fst (last sortedFreq), snd (last sortedFreq))]
            else
                introduceLevel 3 sortedFreq

--Helper to adjust lists by moving words with matching frequencies
adjustLists :: [(String, Int)] -> [(String, Int)] -> ([(String, Int)], [(String, Int)])
adjustLists upper lower =
    let upperFreqLimit = if null upper then maxBound else snd (last upper)
        lowerFreqLimit = if null lower then minBound else snd (head lower)
    in if upperFreqLimit == lowerFreqLimit
        then let (moveDown, stayUp) = partition (\(_, freq) -> freq == upperFreqLimit) upper
             in (stayUp, moveDown ++ lower)
        else (upper, lower)

introduceLevel :: Int -> [(String, Int)] -> [(String, Int)]
introduceLevel _ [] = []
introduceLevel level list =
    case level of
        1 -> map (\(word, freq) -> (word, 1)) list
        2 -> map (\(word, freq) -> (word, 2)) list
        3 -> map (\(word, freq) -> (word, 3)) list

--Uses randomization to shuffle the words into random groups
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    i <- randomRIO (0, length xs - 1)
    let (left, (a:right)) = splitAt i xs
    rest <- shuffle (left ++ right)
    return (a : rest)

--Groups words into groups of random sizes
groupWords :: Int -> [a] -> IO [[a]]
groupWords _ [] = return [] 
groupWords total wordsList = do
    size <- randomRIO (2, 5)
    let (chunk, rest) = splitAt size wordsList
    restChunks <- groupWords total rest
    return (chunk : restChunks) 

randomSpacing :: IO [Int]
randomSpacing = do
    seed <- newStdGen
    let spacing = take 10 $ randomRs (1, 5) seed
    return spacing

insertSpacing :: String -> IO String
insertSpacing wordChunk = do
    spaces <- randomSpacing
    let spacedWords = zipWith (\w s -> replicate s ' ' ++ w) (words wordChunk) spaces
    return $ concat spacedWords

--Creates a textual word cloud based on the frequencies of the words.
--There are three importance levels such that the most repeating words appear UPERACASE, the average
--appearing words appear with the first letter Capitalized and the rest appear with all lowercase.
--The cloud groups the words into random groups of shuffled words and adds random spacing between
--them in an effort to produce a word cloud.
--A good representation can be seen when ran with the file "text.txt".
createCloud :: String -> IO ()
createCloud content = do
    let wordsList = transform content
        total = length $ words content
    shuffledWords <- shuffle wordsList
    groupedWords <- groupWords total shuffledWords
    let styledGroups = map (map styleCloud) groupedWords
    spacedGroups <- mapM (fmap concat . mapM insertSpacing) styledGroups
    putStrLn "Here is your word cloud:"
    putStrLn "========================="
    mapM_ putStrLn spacedGroups

--Error Handling--

checkFileExistence :: String -> IO()
checkFileExistence filePath = do
    fileExists <- doesFileExist filePath
    if fileExists
        then do
            content <- readFile filePath `catch` handleReadError
            putStrLn "File Loaded Successfully!"
            if length content == 0 
                then do 
                    putStrLn "Error: Your file is empty" 
                    return () 
                else menu content   
        else do
            putStrLn "Error: The file does not exist!"
            return ()

handleReadError :: IOException -> IO String
handleReadError _ = do
    putStrLn "Error: Unable to read the file"
    return ""

isValidChar :: Char -> Bool
isValidChar c = isAlpha c || c == '-' || c == '\''

getValidInt :: String -> IO Int
getValidInt prompt = do
    putStr prompt
    hFlush stdout
    n <- getLine
    case readMaybe n :: Maybe Int of
        Just x -> do
            if x > 0 
                then return x
                else do
                    putStrLn "Invalid input! Please enter a valid integer."
                    getValidInt prompt
        Nothing -> do
            putStrLn "Invalid input! Please enter a valid integer."
            getValidInt prompt

getValidChoice :: String -> IO()
getValidChoice content = do
    putStr "Would you like to perform n-gram analysis for another value? (yes/no): "
    hFlush stdout
    choice <- getLine
    case choice of
        "yes" -> do
            askForNGram content
        "no" -> menu content
        _ -> do
            putStrLn "Invalid command! Please try again!"
            getValidChoice content

askForFullList :: [(String, Int)] -> IO()
askForFullList frequency = do 
    putStr "Would you like to see the full list? (yes/no): "
    hFlush stdout
    choice <- getLine
    case choice of
        "yes" -> do
            putStrLn "Here is the full list: "
            mapM_ (\(w, fr) -> putStrLn("( " ++ show w ++ " )" ++ " appears " ++ show fr ++ " times")) $ sortOn (negate . snd) frequency
        "no" ->
            return ()
        _ -> do
            putStrLn "Invalid command! Please try again!"
            askForFullList frequency
