module Formatter (highlightErrors) where

import Data.List (intercalate)

redBoldUnderline :: String
redBoldUnderline = "\x1b[1;4;91m"

blueBold :: String
blueBold = "\x1b[1;34m"

reset :: String
reset = "\x1b[0m"

zipT :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
zipT (a1, b1) (a2, b2) = (a1 ++ a2, b1 ++ b2)

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

highlightLine :: String -> [(Int, Int, [String])] -> Int -> (String, [[String]])
highlightLine [] _ _ = ("", [])
highlightLine (x:xs) [] i = zipT ([x], []) $ highlightLine xs [] (i + 1)
highlightLine (x:xs) (err@(start, end, suggestions):rest) i
  | i == start = zipT (redBoldUnderline ++ [x], []) $ highlightLine xs (err:rest) (i + 1)
  | i == end = zipT (reset ++ [x], [suggestions]) $ highlightLine xs rest (i + 1)
  | otherwise = zipT ([x], []) $ highlightLine xs (err:rest) (i + 1)

highlightErrors :: String -> [(Int, Int, [String])] -> String
highlightErrors text errors = highlightErrors' (lines text) errors 0 1
  where
    highlightErrors' :: [String] -> [(Int, Int, [String])] -> Int -> Int -> String
    highlightErrors' [] _ _ _ = ""
    highlightErrors' (line:rest) errors i si =
      let (highlighted, suggestions) = highlightLine line errors i
          suggestionsLines = unlines $ mapInd (\sl i -> "(" ++ (show $ si + i) ++ ") " ++ intercalate ", " sl) suggestions
          nextLines = highlightErrors' rest (drop (length suggestions) errors) (i + length line + 1) (si + length suggestions)
      in highlighted ++ "\n" ++ blueBold ++ suggestionsLines ++ reset ++ nextLines
