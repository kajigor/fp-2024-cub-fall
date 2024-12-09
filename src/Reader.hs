module Reader where

import System.Directory (doesFileExist)
import System.IO.Error (tryIOError)
import Data.List (group, sort, sortBy)
import Data.Ord (comparing, Down(..))
import Data.Char (isAlphaNum)

fileCheck :: FilePath -> IO Bool
fileCheck = doesFileExist

wordCounter :: String -> Int
wordCounter content = length (words content)

lineCounter :: String -> Int
lineCounter content = length (lines content)

characterCounter :: String -> Int
characterCounter content = length content

frequentWord :: String -> String
frequentWord content = mostFrequentWord content

mostFrequentWord :: String -> String
mostFrequentWord content =
    let cleanWords = processContent content
        wordCounts = countOccurrence cleanWords
        sortedWordCounts = sortBy (comparing (Down . snd)) wordCounts
    in if not (null sortedWordCounts)
       then fst (head sortedWordCounts)
       else "No words found"

ngram :: String -> Int -> [(String, Int)]
ngram content n = generateNgrams content n

generateWordCloud :: String -> String
generateWordCloud content =
    let ngrams = generateNgrams content 1
        sortedNgrams = sortBy (comparing (Down . snd)) ngrams
    in unwords $ concatMap (\(word, count) -> replicate count word) sortedNgrams

processFileMaybe :: FilePath -> IO (Maybe String)
processFileMaybe path = do
    result <- tryIOError (readFile path)
    case result of
        Left _  -> return Nothing
        Right content -> return (Just content)

cleanWord :: String -> String
cleanWord = filter isAlphaNum

outputFormatting :: (String, Int) -> String
outputFormatting (sentence, count) = sentence ++ ": " ++ show count ++ " occurrences"

formatNgrams :: [(String, Int)] -> [String]
formatNgrams ngrams = map outputFormatting ngrams

ngramHelper :: Int -> [String] -> [String]
ngramHelper n tokens
    | length tokens < n = []
    | otherwise = unwords (take n tokens) : ngramHelper n (tail tokens)

generateOccurrences :: Int -> [String] -> [(String, Int)]
generateOccurrences n wordsList =
    let ngrams = ngramHelper n wordsList
    in countOccurrence ngrams

generateNgrams :: String -> Int -> [(String, Int)]
generateNgrams content n =
    let cleanWords = processContent content
    in generateOccurrences n cleanWords

processContent :: String -> [String]
processContent content = map cleanWord (words content)

countOccurrence :: Ord a => [a] -> [(a, Int)]
countOccurrence xs = map (\ws -> (head ws, length ws)) (group . sort $ xs)