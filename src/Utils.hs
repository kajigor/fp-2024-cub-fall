module Utils (getWords, wrap) where
import Data.Char (isAlpha)

getWords' :: String -> Int -> String -> [(Int, Int, String)]
getWords' [] _ _ = []
getWords' (x:xs) i word
  | isAlpha x = getWords' xs (i + 1) (word ++ [x])
  | length word > 0 = (i - length word, i, word) : getWords' xs (i + 1) ""
  | otherwise = getWords' xs (i + 1) ""

getWords :: String -> [(Int, Int, String)]
getWords text = getWords' text 0 ""

wrap :: Int -> String -> String
wrap maxWidth text = unlines $ map (wrap' "" "") $ lines text
  where
    wrap' currLine currWord []
      | length currLine + 1 + length currWord <= maxWidth = currLine ++ " " ++ currWord
      | otherwise = currLine ++ "\n" ++ currWord
    wrap' currLine currWord (' ':xs)
      | length (currLine ++ " " ++ currWord) <= maxWidth || currLine == "" =
        let newLine = if currLine == "" then currWord else (currLine ++ " " ++ currWord)
         in wrap' newLine "" xs
      | otherwise = currLine ++ "\n" ++ (wrap' currWord "" xs)
    wrap' currLine currWord (x:xs) = wrap' currLine (currWord ++ [x]) xs
