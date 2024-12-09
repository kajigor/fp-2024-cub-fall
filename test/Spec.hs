import Reader
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

genString :: Gen String
genString = Gen.string (Range.linear 0 100) Gen.unicode

genMultiLineString :: Gen String
genMultiLineString = do
    liness <- Gen.list (Range.linear 1 100) (Gen.string (Range.linear 0 50) Gen.unicode)
    return (unlines liness)

prop_nonNegativeWordCount :: Property
prop_nonNegativeWordCount = property $ do
    content <- forAll genString
    let result = wordCounter content
    assert (result >= 0)

prop_lineCount :: Property
prop_lineCount = property $ do
    content <- forAll genMultiLineString
    let newlineCount = length (filter (== '\n') content)
        expectedLineCount = if null content || last content == '\n' then newlineCount else newlineCount + 1
        resultLinesCount = lineCounter content
    assert (resultLinesCount == expectedLineCount)

prop_nonNegativeCharacterCount :: Property
prop_nonNegativeCharacterCount = property $ do
  content <- forAll genString
  let result = characterCounter content
  assert (result >= 0)

prop_Ngram_largeN :: Property
prop_Ngram_largeN = property $ do
    content <- forAll genString
    let n = length (words content) + 1
    let result = ngram content n
    assert (null result)

prop_Ngram_emptyContent :: Property
prop_Ngram_emptyContent = property $ do
    let content = ""
    n <- forAll (Gen.int (Range.linear 1 10))
    let result = ngram content n
    assert (null result)

testNgram :: [TestTree]
testNgram =  
    [ testCase "Single word" $
        ngram "hello" 1 @?= [("hello", 1)]
    , testCase "Multiple words with frequencies" $
        (take 1 (ngram "Hello world Hello world Yassine" 2)) @?= [("Hello world", 2)]
  ]

testFrequentWord :: [TestTree]
testFrequentWord =  
    [ testCase "Empty string" $
        frequentWord "" @?= "No words found"
    , testCase "Single word" $
        frequentWord "hello" @?= "hello"
    , testCase "Multiple words with frequencies" $
        frequentWord "apple banana apple orange banana banana" @?= "banana"
    , testCase "Handles punctuation correctly" $
        frequentWord "hello, apple!" @?= "apple"
    , testCase "If same frequency for multiple words" $
        frequentWord "hello yassine founounou" @?= "founounou"
  ]

test_generateWordCloud :: [TestTree]
test_generateWordCloud =
  [ testCase "Empty string" $
      generateWordCloud "" @?= ""
  , testCase "Single word" $
      generateWordCloud "hello" @?= "hello"
  , testCase "Multiple words with frequencies" $
      generateWordCloud "apple banana apple orange banana banana" @?= "banana banana banana apple apple orange"
  , testCase "Handles punctuation correctly" $
      generateWordCloud "hello, hello world!" @?= "hello hello world"
  ]

testNgramGroup :: TestTree
testNgramGroup = testGroup "Ngram tests" testNgram

test_generateWordCloudGroup :: TestTree
test_generateWordCloudGroup = testGroup "generateWordCloud tests" test_generateWordCloud

testFrequentWordGroup :: TestTree
testFrequentWordGroup = testGroup "frequentWord tests" testFrequentWord

tests :: TestTree
tests = testGroup "Reader Module Tests"
  [ testProperty "WordCounter always generate result larger or equal than 0" prop_nonNegativeWordCount
  , testProperty "lineCounter counts lines properly" prop_lineCount
  , testProperty "characterCounter always generates result larger than or equal than 0" prop_nonNegativeCharacterCount
  , testProperty "Ngram with n larger than the number of words" prop_Ngram_largeN
  , testProperty "Ngram with empty content" prop_Ngram_emptyContent
  , test_generateWordCloudGroup
  , testFrequentWordGroup
  , testNgramGroup
  ]

main :: IO ()
main = defaultMain tests