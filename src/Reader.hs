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
        groupedWords = group $ sort cleanWords
        wordCounts = map (\ws -> (head ws, length ws)) groupedWords
        sortedWordCounts = sortBy (comparing (Down . snd)) wordCounts
    in if not (null sortedWordCounts)
       then fst (head sortedWordCounts)
       else "No words found"

ngram :: String -> Int -> [(String, Int)]
ngram content n =
    let cleanWords = processContent content
        sortedNgrams = generateOccurrences n cleanWords
    in sortedNgrams

generateWordCloud :: String -> String
generateWordCloud content =
    let cleanWords = processContent content
        sortedNgrams = generateOccurrences 1 cleanWords
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
outputFormatting (sentence, count) = sentence ++ ": " ++ show count ++ " occurences"

formatNgrams :: [(String, Int)] -> [String]
formatNgrams ngrams = map outputFormatting ngrams

ngramHelper :: Int -> [String] -> [String]
ngramHelper n tokens
    | length tokens < n = []
    | otherwise =  unwords (take n tokens) : (ngramHelper n (tail tokens))

generateOccurrences :: Int -> [String] -> [(String, Int)]
generateOccurrences n wordsList =
    let ngrams = ngramHelper n wordsList
        occurrences = countOccurence ngrams
    in sortBy (comparing (Down . snd)) occurrences

processContent :: String -> [String]
processContent content = map cleanWord (words content)

countOccurence :: [String] -> [(String, Int)]
countOccurence ngrams = map (\ws -> (head ws, length ws)) ((group . sort) ngrams)