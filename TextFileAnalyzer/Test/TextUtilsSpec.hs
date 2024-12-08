module TextUtilsSpec (tests) where

import Test.HUnit
import TextUtils (tokenizeWords)

tests :: Test
tests = TestList
    [ TestLabel "Test Empty After Punctuation" testEmptyAfterPunctuation
    , TestLabel "Test Mixed Words and Punctuation" testMixedWordsAndPunctuation
    ]

testEmptyAfterPunctuation :: Test
testEmptyAfterPunctuation = TestCase $ do
    let input = "   !!!   ???   "
        result = tokenizeWords input
    assertEqual "Should return empty list for punctuation only" [] result

testMixedWordsAndPunctuation :: Test
testMixedWordsAndPunctuation = TestCase $ do
    let input = "Hello, world!! This - is a test???"
        result = tokenizeWords input
    assertEqual "Should return only valid lower-case words" ["hello","world","this","is","a","test"] result
