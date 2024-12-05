import Test.Hspec
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Dictionary as Dict
import ErrorHandler (safeReadFile, safeWriteFile)
import Control.Exception (IOException)

testDict :: Dict.Dictionary
testDict = Map.fromList [(T.pack "hello", True), (T.pack "world", True), (T.pack "hell", True)]

checkWordPropertyInDictionary :: Dict.Dictionary -> T.Text -> Bool
checkWordPropertyInDictionary dict word = 
    let wordClean = Dict.cleanWord word
    in if Map.member wordClean dict 
       then True
       else False

suggestCorrectionsProperties :: Dict.Dictionary -> T.Text -> Bool
suggestCorrectionsProperties dict word =
    let cleanedWord = Dict.cleanWord word
    in if Map.member cleanedWord dict
       then False
       else True

main :: IO ()
main = hspec $ do
    describe "Spell Checke tests" $ do
        it "checks if word is in dictionary" $ do
            Dict.checkWord testDict (T.pack "hello") `shouldBe` True
            Dict.checkWord testDict (T.pack "world") `shouldBe` True
            Dict.checkWord testDict (T.pack "hell") `shouldBe` True
            Dict.checkWord testDict (T.pack "goodbye") `shouldBe` False
        it "suggests corrections" $ do
            let word = T.pack "helt"
            length (Dict.suggestCorrections testDict word) `shouldBe` 3
        it "adds a custom word" $ do
            let newWord = T.pack "newword"
            let updatedDict = Dict.addCustomWord newWord testDict
            Dict.checkWord updatedDict newWord `shouldBe` True
        it "cleans a word" $ do
            let dirtyWord = T.pack "hell@!"
            Dict.cleanWord dirtyWord `shouldBe` T.pack "hell"
        it "splits words" $ do
            let text = T.pack "hello world lets check"
            let words = Dict.splitWords text
            words `shouldBe` [T.pack "hello", T.pack "world", T.pack "lets", T.pack "check"]

    describe "Edge case tests" $ do
        it "should return empty list for suggestions when the word is correct" $ do
            let correctWord = T.pack "hello"
            Dict.suggestCorrections testDict correctWord `shouldBe` []

        it "should handle empty string input for word checking" $ do
            Dict.checkWord testDict (T.pack "") `shouldBe` False

        it "should handle empty string input for word cleaning" $ do
            Dict.cleanWord (T.pack "") `shouldBe` (T.pack "")

        it "should handle non-alphabetic characters in splitWords" $ do
            let text = T.pack "123 !@#"
            Dict.splitWords text `shouldBe` [T.pack "",T.pack ""]

    describe "Property-like Test Part of Dictionary" $ do
        it "should return True if word is in dictionary" $
            checkWordPropertyInDictionary testDict (T.pack "hello") `shouldBe` True

        it "should return False if word is not in dictionary" $
            checkWordPropertyInDictionary testDict (T.pack "goodbye") `shouldBe` False
    
    describe "Property-like Test Suggested Words" $ do
        it "should not suggest corrections if word is in dictionary" $
            suggestCorrectionsProperties testDict (T.pack "hello") `shouldBe` False

        it "should suggest corrections if word is not in dictionary" $
            suggestCorrectionsProperties testDict (T.pack "helt") `shouldBe` True



