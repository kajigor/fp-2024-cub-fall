module StatisticsSpec (tests) where

import Test.HUnit
import Test.QuickCheck
import Statistics (computeStatistics, TextStatistics(..))

tests :: Test
tests = TestList
    [ TestLabel "Test Statistics Computation" testStatisticsComputation
    , TestLabel "Property Test: Character Count >= Word Count" prop_charCountGreaterOrEqualWordCount
    , TestLabel "Test Empty Content" testEmptyContent
    , TestLabel "Test Special Characters" testSpecialCharacters
    , TestLabel "Property Test: Non-Negative Counts" prop_nonNegativeCountsTest
    ]

testStatisticsComputation :: Test
testStatisticsComputation = TestCase $ do
    let content = "Hello World\nThis is a test."
        stats = computeStatistics content
    assertEqual "Line count should be 2" 2 (lineCount stats)
    assertEqual "Word count should be 5" 5 (wordCount stats)
    assertEqual "Character count should be 25" 25 (characterCount stats)

prop_charCountGreaterOrEqualWordCount :: Test
prop_charCountGreaterOrEqualWordCount = TestCase $ quickCheck prop
  where
    prop :: String -> Bool
    prop content =
        let stats = computeStatistics content
        in characterCount stats >= wordCount stats

testEmptyContent :: Test
testEmptyContent = TestCase $ do
    let content = ""
        stats = computeStatistics content
    assertEqual "Line count 0" 0 (lineCount stats)
    assertEqual "Word count 0" 0 (wordCount stats)
    assertEqual "Character count 0" 0 (characterCount stats)

testSpecialCharacters :: Test
testSpecialCharacters = TestCase $ do
    let content = "@#$%^&*()_+!"
        stats = computeStatistics content
    assertEqual "Line count should be 1" 1 (lineCount stats)
    assertEqual "Word count should be 1" 1 (wordCount stats)
    assertEqual "Character count matches length" (length content) (characterCount stats)

prop_nonNegativeCountsTest :: Test
prop_nonNegativeCountsTest = TestCase $ quickCheck prop
  where
    prop :: String -> Bool
    prop content =
        let stats = computeStatistics content
        in lineCount stats >= 0 && wordCount stats >= 0 && characterCount stats >= 0
