module Main (main) where

import Test.Hspec
import FailCont.FailCont
import qualified FailCont.Main as Main  

runAndEval :: FailCont (Either String Int) String Int -> Either String Int
runAndEval = evalFailCont

main :: IO ()  
main = hspec $ do
  describe "FailCont" $ do
    it "fmap (+1) applied to a success value in FailCont" $ do
      let fc = FailCont $ \s _ -> s (42 :: Int)
      runAndEval (fmap (+1) fc) `shouldBe` (Right 43 :: Either String Int)
    it "Applicative instance works as expected" $ do
      let fc1 = FailCont $ \s _ -> s ((+) 1)  
          fc2 = FailCont $ \s _ -> s (42 :: Int)
      runAndEval (fc1 <*> fc2) `shouldBe` (Right 43 :: Either String Int)
    it "Monad instance works as expected" $ do
      let fc = FailCont $ \s _ -> s (42 :: Int)
          f = \x -> FailCont $ \s _ -> s (x + 1)
      runAndEval (fc >>= f) `shouldBe` (Right 43 :: Either String Int)

  describe "addInts" $ do
    it "adds two valid integers" $ do
      evalFailCont (Main.addInts "13" "42") `shouldBe` Right 55
    it "returns EmptyInput on empty input" $ do
      evalFailCont (Main.addInts "" "42") `shouldBe` Left Main.EmptyInput
    it "returns ParseFailed on invalid input" $ do
      evalFailCont (Main.addInts "13" "fourty two") `shouldBe` Left (Main.ParseFailed "fourty two")

  describe "divInts" $ do
    it "divides two valid integers" $ do
      evalFailCont (Main.divInts "42" "13") `shouldBe` Right 3
    it "returns DivisionByZero on division by zero" $ do
      evalFailCont (Main.divInts "13" "0") `shouldBe` Left Main.DivisionByZero
    it "returns DivisionByZero on division by zero with leading zeros" $ do
      evalFailCont (Main.divInts "13" "000") `shouldBe` Left Main.DivisionByZero
    it "returns EmptyInput on empty input" $ do
      evalFailCont (Main.divInts "" "42") `shouldBe` Left Main.EmptyInput
