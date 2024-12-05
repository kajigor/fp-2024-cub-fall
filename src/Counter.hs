module Counter (Counter (Counter, total), getCount, fromList) where

import qualified Data.HashMap.Strict as HashMap

data Counter = Counter
  { wordCount :: HashMap.HashMap String Int,
    total :: Int
  }
  deriving (Show, Eq)

getCount :: Counter -> String -> Int
getCount counter word = HashMap.lookupDefault 0 word (wordCount counter)

fromList :: [String] -> Counter
fromList allWords =
  Counter
    { wordCount = HashMap.fromListWith (+) [(word, 1) | word <- allWords],
      total = length allWords
    }
