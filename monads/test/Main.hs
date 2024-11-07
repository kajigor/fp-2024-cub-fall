module Main (main) where
import Test.Hspec
import FailCont.FailCont
import qualified FailCont.Main as Main
import qualified FailCont.EmailValidator as EV

runAndEval :: FailCont (Either String Int) String Int -> Either String Int
runAndEval = evalFailCont

main :: IO ()  
main = hspec $ do
  describe "FailCont" $ do
    it "fmap (+1) applied to a success value in FailCont" $ do
      let fc = FailCont $ \s _ -> s (42 :: Int)
      runAndEval (fmap (+1) fc) `shouldBe` (Right 43 :: Either String Int)
    it "Applicative instance works" $ do
      let fc1 = FailCont $ \s _ -> s ((+) 1)  
          fc2 = FailCont $ \s _ -> s (42 :: Int)
      runAndEval (fc1 <*> fc2) `shouldBe` (Right 43 :: Either String Int)
    it "Monad instance works" $ do
      let fc = FailCont $ \s _ -> s (42 :: Int)
          f = \x -> FailCont $ \s _ -> s (x + 1)
      runAndEval (fc >>= f) `shouldBe` (Right 43 :: Either String Int)

  describe "addInts" $ do
    it "add two valid integers" $ do
      evalFailCont (Main.addInts "13" "42") `shouldBe` Right 55
    it "return EmptyInput on empty input" $ do
      evalFailCont (Main.addInts "" "42") `shouldBe` Left Main.EmptyInput
    it "return ParseFailed on invalid input" $ do
      evalFailCont (Main.addInts "13" "fourty two") `shouldBe` Left (Main.ParseFailed "fourty two")

  describe "divInts" $ do
    it "divide two valid integers" $ do
      evalFailCont (Main.divInts "42" "13") `shouldBe` Right 3
    it "return DivisionByZero on division by zero" $ do
      evalFailCont (Main.divInts "13" "0") `shouldBe` Left Main.DivisionByZero
    it "return DivisionByZero on division by zero with leading zeros" $ do
      evalFailCont (Main.divInts "13" "000") `shouldBe` Left Main.DivisionByZero
    it "return EmptyInput on empty input" $ do
      evalFailCont (Main.divInts "" "42") `shouldBe` Left Main.EmptyInput

  describe "EV" $ do
    it "validates correct email" $ do
      evalFailCont (EV.validateEmail "test@example.com") `shouldBe` Right "test@example.com"
    it "fails on empty email" $ do
      evalFailCont (EV.validateEmail "") `shouldBe` Left EV.EmptyEmail
    it "fails on missing @ symbol" $ do
      evalFailCont (EV.validateEmail "test.email") `shouldBe` Left EV.NoAtSymbol
    it "fails on missing local part" $ do
      evalFailCont (EV.validateEmail "@domain.com") `shouldBe` Left EV.NoLocalPart
    it "fails on missing domain part" $ do
      evalFailCont (EV.validateEmail "test@") `shouldBe` Left EV.NoDomainPart
    it "fails on invalid local part" $ do
      evalFailCont (EV.validateEmail "test!@example.com") `shouldBe` Left (EV.InvalidLocalPart "test!")
    it "fails on missing TLD" $ do
      evalFailCont (EV.validateEmail "test@domain") `shouldBe` Left EV.MissingTLD
    it "fails on invalid TLD" $ do
      evalFailCont (EV.validateEmail "test@domain.c") `shouldBe` Left (EV.InvalidTLD "c")