module NGramAnalysisSpec (tests) where

import Test.HUnit
import NGramAnalysis (generateNGrams)

tests :: Test
tests = TestList
    [ TestLabel "Test N-Gram Generation" testNGramGeneration
    , TestLabel "Test N Greater Than Word Count" testNGreaterThanWordCount
    , TestLabel "Test Repeated Phrases" testRepeatedPhrases
    ]

testNGramGeneration :: Test
testNGramGeneration = TestCase $ do
    let content = "i love jetrbains and bremen"
        ngrams = generateNGrams 2 content
    assertBool "N-grams should not be empty" (not (null ngrams))

testNGreaterThanWordCount :: Test
testNGreaterThanWordCount = TestCase $ do
    let content = "one two"
        ngrams = generateNGrams 3 content
    assertEqual "Empty list when n > word count" [] ngrams

testRepeatedPhrases :: Test
testRepeatedPhrases = TestCase $ do
    let content = "i love jetrbains i love jetbrains i love jetbrains"
        ngrams = generateNGrams 3 content
    assertBool "Should have non-empty n-grams" (not (null ngrams))
    let (topNGram, freq) = head ngrams
    assertEqual "Most frequent n-gram" ("i love jetrbains", 3) (topNGram, freq)
