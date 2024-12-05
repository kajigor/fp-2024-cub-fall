module WordCloud (generateWordCloud, displayWordCloud) where

import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import TextUtils (tokenizeWords)

generateWordCloud :: String -> [(String, Int)]
generateWordCloud content = map scaleFrequency sortedWordCounts
  where
    wordsList = tokenizeWords content
    wordCounts = Map.fromListWith (+) [(word, 1) | word <- wordsList]
    sortedWordCounts = take 20 $ sortOn (negate . snd) (Map.toList wordCounts)
    maxCount = if null sortedWordCounts then 1 else snd (head sortedWordCounts)
    scaleFrequency (word, count) = (word, scale count maxCount)

scale :: Int -> Int -> Int
scale count maxCount = (count * 10) `div` maxCount

displayWordCloud :: [(String, Int)] -> IO ()
displayWordCloud wordCloud = do
    putStrLn "\nWord Cloud:"
    mapM_ (\(word, scaledCount) -> putStrLn $ replicate scaledCount '*' ++ " " ++ word) wordCloud
