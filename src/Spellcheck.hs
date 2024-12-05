-- https://norvig.com/spell-correct.html

module Spellcheck (edits1, candidates, spellcheck) where

import Data.Function (on)
import Data.List (sortOn)
import qualified Dictionary
import Dictionary (Dictionary)
import Utils
import Prelude hiding (words)
import Data.Char (toLower)
import qualified Data.HashSet as HashSet

known :: Dictionary -> HashSet.HashSet String -> HashSet.HashSet String
known dict = HashSet.intersection (Dictionary.getKeys dict)

edits1 :: String -> HashSet.HashSet String
edits1 word = HashSet.fromList (deletes ++ transposes ++ replaces ++ inserts)
  where
    splits = [splitAt i word | i <- [0 .. length word]]
    deletes = [l ++ tail r | (l, r) <- splits, not (null r)]
    transposes = [l ++ [head (tail r)] ++ [head r] ++ drop 2 r | (l, r) <- splits, length r > 1]
    replaces = [l ++ [c] ++ tail r | (l, r) <- splits, not (null r), c <- ['a' .. 'z'], c /= head r]
    inserts = [l ++ [c] ++ r | (l, r) <- splits, c <- ['a' .. 'z']]

rawCandidates :: Dictionary -> String -> [String]
rawCandidates dict word =
  let e1Raw = edits1 word
      e1 = known dict e1Raw
      e2 = HashSet.unions [known dict (edits1 e) | e <- HashSet.toList e1Raw]
   in if (Dictionary.hasWord dict word)
      then [word]
      else if not $ null e1
      then HashSet.toList e1
      else if not $ null e2
      then HashSet.toList e2
      else [word]

prob :: Dictionary -> String -> Double
prob dict word = fromIntegral (Dictionary.getWordCount dict word) / fromIntegral (Dictionary.getTotal dict)

candidates :: Dictionary -> String -> Maybe [String]
candidates dict word =
  let sorted = sortOn (negate . (prob dict)) (rawCandidates dict word)
   in if sorted == [word] then Nothing else Just $ take 5 sorted

spellcheck :: Dictionary -> String -> [(Int, Int, [String])]
spellcheck dict text = do
  let wordsWithPositions = getWords $ map toLower text
  let corrected = [(start, end, candidates dict word) | (start, end, word) <- wordsWithPositions]
  [(start, end, words) | (start, end, Just words) <- corrected]
