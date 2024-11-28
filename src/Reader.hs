module Reader where

import System.Directory (doesFileExist)
import Data.List (group, sort, sortBy)
import Data.Ord (comparing, Down(..))
import Data.Char (isAlphaNum)

fileCheck :: FilePath -> IO Bool
fileCheck = doesFileExist

wordCounter :: String -> IO Int
wordCounter path = return (length (words path))

lineCounter :: String -> IO Int
lineCounter path = return (length (lines path)) 

characterCounter :: String -> IO Int
characterCounter path = return (length path )

frequentWord :: String -> IO String
frequentWord path = return (mostFrequentWord path)

mostFrequentWord :: String -> String
mostFrequentWord content =
    let wordslist = words content
        cleanWords = map cleanWord wordslist
        sortedWords = sort cleanWords
        groupedWords = group sortedWords
        wordCounts = map (\ws -> (head ws, length ws)) groupedWords
        sortedWordCounts = sortBy (comparing (Down . snd)) wordCounts
    in if not (null sortedWordCounts)
       then fst (head sortedWordCounts)
       else "No words found"

ngram :: String -> Int -> IO ()
ngram content n = do
    let ngrams = ngramHelper n (words content)
        occurence = countOccurence ngrams
        sortedNgrams = sortBy (comparing (Down . snd)) occurence
        formatted = map outputFormatting sortedNgrams
    mapM_ putStrLn formatted

generateWordCloud :: String -> String
generateWordCloud content =
    let wordslist = words content
        cleanWords = map cleanWord wordslist
        ngrams = ngramHelper 1 cleanWords
        occurence = countOccurence ngrams
        sortedNgrams = sortBy (comparing (Down . snd)) occurence
    in unwords $ concatMap (\(word, count) -> replicate count word) sortedNgrams

couldRepresentation :: String -> IO ()
couldRepresentation content = putStrLn (generateWordCloud content)

processFileMaybe :: FilePath -> IO (Maybe String)
processFileMaybe path = do
    content <- readFile path
    if null content
        then return Nothing
        else return (Just content)

ngramHelper :: Int -> [String] -> [String]
ngramHelper n tokens
    | length tokens < n = []
    | otherwise =  unwords (take n tokens) : (ngramHelper n (tail tokens))

countOccurence :: [String] -> [(String, Int)]
countOccurence ngrams = map (\ws -> (head ws, length ws)) ((group . sort) ngrams)

outputFormatting :: (String, Int) -> String
outputFormatting (sentence, count) = sentence ++ ": " ++ show count ++ " occurences"

cleanWord :: String -> String
cleanWord = filter isAlphaNum