module FrequentWordsSpec (tests) where

import Test.HUnit
import FrequentWords (findMostFrequentWords)
import Data.List (sortOn)

tests :: Test
tests = TestList
    [ TestLabel "Test Most Frequent Words" testMostFrequentWords
    , TestLabel "Test Tie Frequencies" testTieFrequencies
    , TestLabel "Test Punctuation and Numbers" testPunctuationAndNumbers
    ]

testMostFrequentWords :: Test
testMostFrequentWords = TestCase $ do
    let content = "hello world hello test test test"
        fw = findMostFrequentWords 2 content
    assertEqual "Should return the two most frequent words"
        [("test", 3), ("hello", 2)] fw

testTieFrequencies :: Test
testTieFrequencies = TestCase $ do
    let content = "apple banana apple banana cherry"
        fw = findMostFrequentWords 2 content
        sortedFw = sortOn fst fw
    assertEqual "Two words tied with frequency 2"
        [("apple",2), ("banana",2)] (sortOn fst sortedFw)

testPunctuationAndNumbers :: Test
testPunctuationAndNumbers = TestCase $ do
    let content = "hello, world! 123 123"
        fw = findMostFrequentWords 2 content
    assertEqual "Handle punctuation and numbers"
        [("123",2), ("hello",1)] fw
