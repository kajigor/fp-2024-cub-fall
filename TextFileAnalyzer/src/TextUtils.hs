module TextUtils (tokenizeWords) where

import Data.Char (toLower, isAlphaNum)

tokenizeWords :: String -> [String]
tokenizeWords = map cleanWord . words
  where
    cleanWord = map toLower . filter isAlphaNum
