import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog 
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.List
import System.Directory (getCurrentDirectory, doesFileExist)
import Data.Char
import Control.Monad (when)
import Lib

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Word Count Test 1" $ do
    content <- readFile "test1.txt"
    wordCount content @?= 7

  , testCase "Word Count Test Empty" $ do
      content <- readFile "test_empty.txt"
      wordCount content @?= 0

  , testCase "Lines Count Test 1" $ do
      content <- readFile "test2.txt"
      linesCount content @?= 4

  , testCase "Lines Count Test Empty" $ do
      content <- readFile "test_empty.txt"
      linesCount content @?= 0

  , testCase "Character Count Test 1" $ do
      content <- readFile "test3.txt"
      charCount content @?= 10

  , testCase "Character Count Test 2" $ do
      content <- readFile "test2.txt"
      charCount content @?= 23

  , testCase "Character Count Empty" $ do
      content <- readFile "test_empty.txt"
      charCount content @?= 0

  , testCase "Most Frequent Word Test" $ do
      content <- readFile "test_common_words.txt"
      let expected = [("apple", 4), ("banana", 2), ("carrot", 1), ("grape", 1), ("orange", 1)]
      commonWordsHelper content @?= expected

  , testCase "Most Frequent Word Test Empty" $ do
      content <- readFile "test_empty.txt"
      let expected = []
      commonWordsHelper content @?= expected

  , testCase "Generate N-Gram Test 1" $ do
      content <- readFile "test_ngram.txt"
      let wordsList = map (filter isValidChar) (words (map toLower content))
      let expected = ["the quick","quick brown","brown fox"]
      generateNGrams 2 wordsList @?= expected

  , testCase "Generate N-Gram Test 2" $ do
      content <- readFile "test_empty.txt"
      let wordsList = map (filter isValidChar) (words (map toLower content))
      let expected = []
      generateNGrams 2 wordsList @?= expected

  , testCase "Capitalize Test 1" $ do
      let expected = "Helloworld"
      capitalize "helloworld" @?= expected

  , testCase "Capitalize Test 2" $ do
      let expected = "Hello,"
      capitalize "hello," @?= expected

  , testCase "Capitalize Test Empty" $ do
      let expected = ""
      capitalize "" @?= expected

  , testCase "Style Cloud Test" $ do
      let list = [("apple", 1), ("pineapple", 2), ("banana", 3)]
      let expected = ["apple", "Pineapple", "BANANA"]
      let result = map styleCloud list
      result @?= expected

  , testCase "Test empty string Transform" $
      transform "" @?= []

  , testCase "Test single word Transform" $
      transform "hello" @?= [("hello", 3)]

  , testCase "Test multiple occurrences of the same word Transform" $
      transform "hello hello hello" @?= [("hello", 3)]
 
  , testCase "Test two words Test 1 Transform" $
      transform "hello world" @?= [("hello", 1), ("world", 1)]
    
  , testCase "Test two words Test 1 Transform" $
      transform "hello hello world" @?= [("hello", 3), ("world", 1)]  

  , testCase "Multiple occurences of different words Transform" $ do
      content <- readFile "test_common_words.txt"
      let expected = [("apple",3),("banana",2),("carrot",1),("grape",1),("orange",1)]
      let result = transform content
      result @?= expected

  , testCase "Shuffle and groupWords test" $ do
      let wordsList = [("hello", 3), ("world", 2), ("example", 1)]
      shuffledWords <- shuffle wordsList
      groupedWords <- groupWords (length wordsList) shuffledWords
      length (concat groupedWords) @?= length wordsList
      all (`elem` wordsList) (concat groupedWords) @?= True

  ]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ testProperty "Word Count Non-Negative" $
      property $ do
        content <- forAll $ Gen.string (Range.linear 0 1000) Gen.unicode
        (wordCount content >= 0) === True

  , testProperty "Line Count Non-Negative" $
      property $ do
        content <- forAll $ Gen.string (Range.linear 0 1000) Gen.unicode
        (linesCount content >= 0) === True

  , testProperty "Character Count Correctness" $
      property $ do
        content <- forAll $ Gen.string (Range.linear 0 1000) Gen.unicode
        charCount content === length content

  , testProperty "Frequencies are non-negative CommonWords" $
      property $ do
        content <- forAll $ Gen.string (Range.linear 0 1000) Gen.unicode
        let frequencies = commonWordsHelper content
        all (\(_, count) -> count >= 0) frequencies === True

  , testProperty "Frequencies are non-negative N-Gram" $
      property $ do
        content <- forAll $ Gen.string (Range.linear 0 1000) Gen.unicode
        n <- forAll $ Gen.int (Range.linear 1 10)
        let frequencies = nGramHelper content n
        all (\(_, count) -> count >= 0) frequencies === True

  , testProperty "Words in output are derived from input CommonWords" $
      property $ do
        content <- forAll $ Gen.string (Range.linear 0 1000) Gen.unicode
        let inputWords = map (filter isValidChar . map toLower) (words content)
        let outputWords = map fst (commonWordsHelper content)
        Hedgehog.assert $ all (`elem` inputWords) outputWords

  , testProperty "Words in output are derived from input N-Gram" $
      property $ do
        content <- forAll $ Gen.string (Range.linear 0 1000) Gen.unicode
        let inputWords = map (filter isValidChar . map toLower) (words content)
        let outputWords = map fst (commonWordsHelper content)
        Hedgehog.assert $ all (`elem` inputWords) outputWords
    
  , testProperty "Frequency matches word occurrences CommonWords" $
      property $ do
        content <- forAll $ Gen.string (Range.linear 0 1000) Gen.unicode
        let frequencies = commonWordsHelper content
        let inputWords = map (filter isValidChar . map toLower) (words content)
        if null inputWords
            then frequencies === []
            else all (\(word, freq) -> freq == length (filter (== word) inputWords)) frequencies === True

  , testProperty "Frequency matches n-gram occurrences nGrams" $
     property $ do
        content <- forAll $ Gen.string (Range.linear 0 1000) Gen.unicode
        n <- forAll $ Gen.int (Range.linear 1 10)
        let inputWords = map (filter isValidChar . map toLower) (words content)
        let nGrams = generateNGrams n inputWords
        let frequencies = nGramHelper content n
        if null nGrams
            then frequencies === []
            else do
                annotateShow (content, n, nGrams, frequencies)
                Hedgehog.assert $
                    all (\(ngram, freq) -> freq == length (filter (== ngram) nGrams)) frequencies

  , testProperty "Generate proper n-grams" $
     property $ do
        content <- forAll $ Gen.string (Range.linear 0 1000) Gen.unicode
        let wordsList = map (filter isValidChar) (words (map toLower content))
        annotateShow content
        annotateShow wordsList
        if null wordsList
            then wordsList === []
            else do
                let n = 2
                let nGrams = generateNGrams n wordsList
                annotateShow nGrams
                Hedgehog.assert $ all (\ngram -> length (words ngram) == n) nGrams


  , testProperty "styleCloud preserves word length" $
     property $ do
        word <- forAll $ Gen.string (Range.linear 0 50) Gen.alpha
        importance <- forAll $ Gen.element [1, 2, 3]
        let styled = styleCloud (word, importance)
        length styled === length word

  , testProperty "styleCloud is idempotent for transformations" $
     property $ do
        word <- forAll $ Gen.string (Range.linear 0 50) Gen.unicode
        importance <- forAll $ Gen.element [1, 2, 3]
        let styled = styleCloud (word, importance)
        styleCloud (styled, importance) === styled

  , testProperty "Words are correctly assigned to levels Transform" $
     property $ do
        content <- forAll $ Gen.string (Range.linear 0 1000) Gen.unicode
        let transformed = transform content
            levels = map snd transformed
        levels === sortBy (compare) levels

  , testProperty "Transform function is consistent" $
     property $ do
        content <- forAll $ Gen.string (Range.linear 0 1000) Gen.unicode
        let transformed1 = transform content
            transformed2 = transform content
        transformed1 === transformed2

  , testProperty "Transform preserves word count" $
     property $ do
        content <- forAll $ Gen.string (Range.linear 0 1000) Gen.unicode
        let transformed = transform content
            originalWords = map (filter isValidChar . map toLower) (words content)
        length transformed === length (nub originalWords)
  ]

main :: IO ()
main = do
    prepareTestFiles
    defaultMain $ testGroup "All Tests" [unitTests, propertyTests]

prepareTestFiles :: IO ()
prepareTestFiles = do
    let testFiles = [ ("test1.txt", "hello world\nthis is a test\n\nHelloWorld")
                    , ("test2.txt", "line1\nline2\nline3\nline4")
                    , ("test3.txt", "characters")
                    , ("test_empty.txt", "")
                    , ("test_common_words.txt", "apple banana banana orange apple apple grape carrot apple")
                    , ("test_ngram.txt", "the quick brown fox")
                    ]
    mapM_ createFileIfNotExists testFiles

createFileIfNotExists :: (FilePath, String) -> IO ()
createFileIfNotExists (fileName, content) = do
    exists <- doesFileExist fileName
    when (not exists) $ writeFile fileName content
