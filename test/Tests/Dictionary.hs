module Tests.Dictionary (testDictionary) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, (@?=))
import qualified Data.HashSet as HashSet
import qualified Dictionary
import Dictionary (Dictionary)

testDictionary :: TestTree
testDictionary =
  testGroup "Dictionary" [testLoad, testAdd, testRemove, testHasWord]
  where
    testLoad = testCase "load" $ do
        let Just dict = Dictionary.fromJsonString "{\"hello\": 1, \"world\": 2}"
        assertEqual "There is 1 \"hello\"" (Dictionary.getWordCount dict "hello") 1
        assertEqual "There are 2 \"world\"" (Dictionary.getWordCount dict "world") 2
        assertEqual "There are 3 words in total" (Dictionary.getTotal dict) 3
        assertEqual "The keys are [\"hello\", \"world\"]" (Dictionary.getKeys dict) (HashSet.fromList ["hello", "world"])
    testAdd = testCase "add" $ do
        let Just dict = Dictionary.fromJsonString "{\"hello\": 1, \"world\": 2}"
        let dict' = Dictionary.addWord dict "test"
        assertEqual "There is 1 \"test\"" (Dictionary.getWordCount dict' "test") 1
        assertEqual "The keys are [\"hello\", \"world\", \"test\"]" (Dictionary.getKeys dict') (HashSet.fromList ["hello", "world", "test"])
        assertEqual "There are 4 words in total" (Dictionary.getTotal dict') 4
    testRemove = testCase "remove" $ do
        let Just dict = Dictionary.fromJsonString "{\"hello\": 1, \"world\": 2}"
        let dict' = Dictionary.removeWord dict "world"
        assertEqual "There is 1 \"hello\"" (Dictionary.getWordCount dict' "hello") 1
        assertEqual "There are 0 \"world\"" (Dictionary.getWordCount dict' "world") 0
        assertEqual "The keys are [\"hello\"]" (Dictionary.getKeys dict') (HashSet.fromList ["hello"])
        assertEqual "There are 1 words in total" (Dictionary.getTotal dict') 1
    testHasWord = testCase "hasWord" $ do
        let Just dict = Dictionary.fromJsonString "{\"hello\": 1, \"world\": 2}"
        assertEqual "There is a \"hello\"" (Dictionary.hasWord dict "hello") True
        assertEqual "There is a \"world\"" (Dictionary.hasWord dict "world") True
        assertEqual "There is no \"test\"" (Dictionary.hasWord dict "test") False
        let dict' = Dictionary.removeWord dict "world"
        assertEqual "There is a \"hello\" in the new dictionary" (Dictionary.hasWord dict' "hello") True
        assertEqual "There is no \"world\" in the new dictionary" (Dictionary.hasWord dict' "world") False
        assertEqual "There is no \"test\" in the new dictionary" (Dictionary.hasWord dict' "test") False
