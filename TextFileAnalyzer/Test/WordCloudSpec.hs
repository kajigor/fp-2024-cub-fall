module WordCloudSpec (tests) where

import Test.HUnit
import WordCloud (generateWordCloud)

tests :: Test
tests = TestList
    [ TestLabel "Test Word Cloud Generation" testWordCloudGeneration
    , TestLabel "Test Empty Content Word Cloud" testEmptyContentWordCloud
    , TestLabel "Test Frequency Scaling" testFrequencyScaling
    ]

testWordCloudGeneration :: Test
testWordCloudGeneration = TestCase $ do
    let content = "hello hello world"
        wc = generateWordCloud content
    assertBool "Should not be empty" (not (null wc))

testEmptyContentWordCloud :: Test
testEmptyContentWordCloud = TestCase $ do
    let content = ""
        wc = generateWordCloud content
    assertEqual "Empty content" [] wc

testFrequencyScaling :: Test
testFrequencyScaling = TestCase $ do
    let content = "a a a a b b b c c d"
        wc = generateWordCloud content
    let expected = [("a",10),("b",7),("c",5),("d",2)]
    assertEqual "Check scaling" expected wc
