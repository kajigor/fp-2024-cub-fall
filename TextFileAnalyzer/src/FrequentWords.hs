module FrequentWords (findMostFrequentWords, displayFrequentWords) where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import TextUtils (tokenizeWords)

findMostFrequentWords :: Int -> String -> [(String, Int)]
findMostFrequentWords n content = take n sortedWordCounts
  where
    wordsList = tokenizeWords content
    wordCounts = Map.fromListWith (+) [(word, 1) | word <- wordsList]
    sortedWordCounts = sortOn (negate . snd) (Map.toList wordCounts)

displayFrequentWords :: [(String, Int)] -> IO ()
displayFrequentWords wordFreqs = do
    putStrLn "\nMost Frequent Words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) wordFreqs

