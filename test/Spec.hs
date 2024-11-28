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

prop_wordCount :: Property
prop_wordCount = property $ do
    content <- forAll genString
    let wordsContent = words content
    resultWordCount <- evalIO $ wordCounter content
    assert (resultWordCount == length wordsContent)

prop_nonNegativeWordCount :: Property
prop_nonNegativeWordCount = property $ do
  content <- forAll genString
  result <- evalIO (wordCounter content)
  assert (result >= 0)

prop_lineCount :: Property
prop_lineCount = property $ do
    content <- forAll genMultiLineString
    let newlineCount = length (filter (== '\n') content)
        expectedLineCount = if null content || last content == '\n' then newlineCount else newlineCount + 1
    resultLinesCount <- evalIO $ lineCounter content
    assert (resultLinesCount == expectedLineCount)

prop_characterCount :: Property
prop_characterCount = property $ do
    content <- forAll genString
    resultCharacterCount <- evalIO $ characterCounter content
    assert (resultCharacterCount == length content)

prop_nonNegativeCharacterCount :: Property
prop_nonNegativeCharacterCount = property $ do
  content <- forAll genString
  result <- evalIO (characterCounter content)
  assert (result >= 0)

testFrequentWord :: [TestTree]
testFrequentWord =  
    [ testCase "Empty string" $
        mostFrequentWord "" @?= "No words found"
    , testCase "Single word" $
        mostFrequentWord "hello" @?= "hello"
    , testCase "Multiple words with frequencies" $
        mostFrequentWord "apple banana apple orange banana banana" @?= "banana"
    , testCase "Handles punctuation correctly" $
        mostFrequentWord "hello, apple!" @?= "apple"
    , testCase "If same frequency for multiple words" $
        mostFrequentWord "hello yassine founounou" @?= "founounou"
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

test_generateWordCloudGroup :: TestTree
test_generateWordCloudGroup = testGroup "generateWordCloud tests" test_generateWordCloud

testFrequentWordGroup :: TestTree
testFrequentWordGroup = testGroup "frequentWord tests" testFrequentWord

tests :: TestTree
tests = testGroup "Reader Module Tests"
  [ testProperty "wordCounter counts words correctly" prop_wordCount
  , testProperty "WordCounter always generate result larger or equal than 0" prop_nonNegativeWordCount
  , testProperty "lineCounter counts lines properly" prop_lineCount
  , testProperty "characterCounter counts characters properly" prop_characterCount
  , testProperty "characterCounter always generates result larger than or equal than 0" prop_nonNegativeCharacterCount
  , test_generateWordCloudGroup
  , testFrequentWordGroup
  ]

main :: IO ()
main = defaultMain tests