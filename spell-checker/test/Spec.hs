{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Hedgehog.Gen (filter)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib
import Data.Char (toLower)
import Data.List (sort, nub)

unit_levenshtein_diff :: IO ()
unit_levenshtein_diff = do
    levenshtein "kitten" "sitting" @?= 3
    levenshtein "apple" "orange" @?= 5
    levenshtein "cat" "dog" @?= 3
    levenshtein "hello" "yellow" @?= 2

unit_suggest_corrections_no_suggestions :: IO ()
unit_suggest_corrections_no_suggestions = do
    let trie = insert "apple" . insert "orange" . insert "banana" $ emptyTrie
    let suggestions = suggestCorrections trie "xyz" 2
    suggestions @?= []
    let suggestions2 = suggestCorrections trie "grapefruit" 2
    suggestions2 @?= []

prop_insert_search :: Property
prop_insert_search = property $ do
    word <- forAll genWord
    let trie = insert word emptyTrie
    search word trie === True

prop_insert_search_non_existent :: Property
prop_insert_search_non_existent = property $ do
    word1 <- forAll genWord
    word2 <- forAll (Gen.filter (/= word1) genWord)
    let trie = insert word1 emptyTrie
    search word2 trie === False

prop_levenshtein_empty :: Property
prop_levenshtein_empty = property $ do
    word <- forAll genWord
    levenshtein "" word === length word
    levenshtein word "" === length word
    levenshtein "" "" === 0

prop_levenshtein_single_char :: Property
prop_levenshtein_single_char = property $ do
    char1 <- forAll $ Gen.string (Range.singleton 1) Gen.alpha
    char2 <- forAll $ Gen.string (Range.singleton 1) Gen.alpha
    if char1 == char2
        then levenshtein char1 char2 === 0
        else levenshtein char1 char2 === 1
    levenshtein char1 "" === 1
    levenshtein "" char2 === 1

prop_suggest_corrections :: Property
prop_suggest_corrections = property $ do
    words <- forAll (Gen.list (Range.linear 1 50) genWord)
    let trie = foldr insert emptyTrie words
    misspelledWord <- forAll genMisspelledWord
    maxDist <- forAll $ Gen.int (Range.linear 1 3)
    let suggestions = suggestCorrections trie misspelledWord maxDist
    assert $ all (`search` trie) suggestions

prop_collectWords :: Property
prop_collectWords = property $ do
  collected <- forAll (Gen.list (Range.linear 1 50) genWord)
  let normalizedCollected = map (map toLower) collected
  let deduplicatedCollected = sort . nub $ normalizedCollected
  let trie = foldr insert emptyTrie normalizedCollected
  let collectedFromTrie = sort (collectWords trie "")
  deduplicatedCollected === collectedFromTrie

prop_levenshtein_self :: Property
prop_levenshtein_self = property $ do
    word <- forAll genWord
    levenshtein word word === 0

prop_levenshtein_symmetric :: Property
prop_levenshtein_symmetric = property $ do
    word1 <- forAll genWord
    word2 <- forAll genWord
    levenshtein word1 word2 === levenshtein word2 word1

prop_suggest_corrections_valid :: Property
prop_suggest_corrections_valid = property $ do
    words <- forAll (Gen.list (Range.linear 1 50) genWord)
    let trie = foldr insert emptyTrie words
    misspelled <- forAll genWord
    let suggestions = suggestCorrections trie misspelled 2
    assert $ all (`search` trie) suggestions

genWord :: Gen String
genWord = Gen.string (Range.linear 1 20) Gen.alpha

genMisspelledWord :: Gen String
genMisspelledWord = do
    word <- genWord
    if length word > 2
        then return $ take (length word - 1) word ++ "x"
        else return "x"  

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testProperty "prop_collectWords" prop_collectWords
  , testProperty "prop_insert_search" prop_insert_search
  , testProperty "prop_levenshtein_self" prop_levenshtein_self
  , testProperty "prop_levenshtein_symmetric" prop_levenshtein_symmetric
  , testProperty "prop_suggest_corrections_valid" prop_suggest_corrections_valid
  , testProperty "prop_insert_search_non_existent" prop_insert_search_non_existent
  , testProperty "prop_levenshtein_empty" prop_levenshtein_empty
  , testProperty "prop_levenshtein_single_char" prop_levenshtein_single_char
  , testProperty "prop_suggest_corrections" prop_suggest_corrections
  , testCase "unit_levenshtein_diff" unit_levenshtein_diff
  , testCase "unit_suggest_corrections_no_suggestions" unit_suggest_corrections_no_suggestions
  ]
