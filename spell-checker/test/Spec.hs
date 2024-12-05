{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib
import Data.Char (toLower)
import Data.List (sort, nub)

unit_insert_search :: IO ()
unit_insert_search = do
    let word1 = "hello"
    let trie1 = insert word1 emptyTrie
    search word1 trie1 @?= True
    let word2 = "world"
    let trie2 = insert word2 emptyTrie
    search word2 trie2 @?= True
    let word3 = "haskell"
    let trie3 = insert word3 emptyTrie
    search word3 trie3 @?= True
    let word4 = "functional"
    let trie4 = insert word4 emptyTrie
    search word4 trie4 @?= True

unit_insert_search_non_existent :: IO ()
unit_insert_search_non_existent = do
    let trie = insert "hello" emptyTrie
    search "world" trie @?= False
    let trie2 = insert "hello" emptyTrie
    search "haskell" trie2 @?= False
    let trie3 = insert "apple" emptyTrie
    search "orange" trie3 @?= False
    let trie4 = insert "test" emptyTrie
    search "exam" trie4 @?= False

unit_levenshtein_self :: IO ()
unit_levenshtein_self = do
    let word = "test"
    levenshtein word word @?= 0
    let word2 = "hello"
    levenshtein word2 word2 @?= 0
    let word3 = "world"
    levenshtein word3 word3 @?= 0

unit_levenshtein_diff :: IO ()
unit_levenshtein_diff = do
    levenshtein "kitten" "sitting" @?= 3
    levenshtein "apple" "orange" @?= 5
    levenshtein "cat" "dog" @?= 3
    levenshtein "hello" "yellow" @?= 2

unit_suggest_corrections :: IO ()
unit_suggest_corrections = do
    let trie = insert "hello" . insert "world" . insert "haskell" . insert "functional" $ emptyTrie
    let suggestions1 = suggestCorrections trie "hellp" 1
    suggestions1 @?= ["hello"]
    let suggestions2 = suggestCorrections trie "wrld" 2
    suggestions2 @?= ["world"]
    let suggestions3 = suggestCorrections trie "hasel" 2
    suggestions3 @?= ["haskell"]
    let suggestions5 = suggestCorrections trie "hellp" 3
    suggestions5 @?= ["hello"]
    let suggestions6 = suggestCorrections trie "worlds" 2
    suggestions6 @?= ["world"]
    let suggestions7 = suggestCorrections trie "heello" 2
    suggestions7 @?= ["hello"]
    let suggestions8 = suggestCorrections trie "funtunal" 4
    suggestions8 @?= ["functional"]
    let suggestions9 = suggestCorrections trie "woorld" 2
    suggestions9 @?= ["world"]

unit_levenshtein_empty :: IO ()
unit_levenshtein_empty = do
    levenshtein "" "hello" @?= 5
    levenshtein "hello" "" @?= 5
    levenshtein "" "" @?= 0

unit_levenshtein_single_char :: IO ()
unit_levenshtein_single_char = do
    levenshtein "a" "a" @?= 0
    levenshtein "a" "b" @?= 1
    levenshtein "a" "" @?= 1
    levenshtein "" "b" @?= 1

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

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testProperty "prop_collectWords" prop_collectWords
  , testProperty "prop_insert_search" prop_insert_search
  , testProperty "prop_levenshtein_self" prop_levenshtein_self
  , testProperty "prop_levenshtein_symmetric" prop_levenshtein_symmetric
  , testProperty "prop_suggest_corrections_valid" prop_suggest_corrections_valid
  , testCase "unit_insert_search" unit_insert_search
  , testCase "unit_insert_search_non_existent" unit_insert_search_non_existent
  , testCase "unit_levenshtein_self" unit_levenshtein_self
  , testCase "unit_levenshtein_diff" unit_levenshtein_diff
  , testCase "unit_suggest_corrections" unit_suggest_corrections
  , testCase "unit_levenshtein_empty" unit_levenshtein_empty
  , testCase "unit_levenshtein_single_char" unit_levenshtein_single_char
  , testCase "unit_suggest_corrections_no_suggestions" unit_suggest_corrections_no_suggestions
  ]
