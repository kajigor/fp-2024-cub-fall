module NGramAnalysis (generateNGrams, displayNGrams) where

import Data.List (tails, sortOn)
import qualified Data.Map.Strict as Map
import TextUtils (tokenizeWords)

type Ngram = Map.Map String Int

generateNGrams :: Int -> String -> [(String, Int)]
generateNGrams n content = take 10 sortedNGramCounts
  where
    wordsList = tokenizeWords content
    ngrams = createNGrams n wordsList
    ngramCounts = countNGrams ngrams
    sortedNGramCounts = sortOn (negate . snd) (Map.toList ngramCounts)

createNGrams :: Int -> [String] -> [String]
createNGrams n ws = [unwords (take n t) | t <- tails ws, length t >= n]

countNGrams :: [String] -> Ngram
countNGrams ngrams = Map.fromListWith (+) [(ngram, 1) | ngram <- ngrams]

displayNGrams :: [(String, Int)] -> IO ()
displayNGrams ngramFreqs = do
    putStrLn "\nTop N-Grams:"
    mapM_ (\(ngram, count) -> putStrLn $ ngram ++ ": " ++ show count) ngramFreqs
