module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import MyRegex
import Parser
import Data.Char (isDigit, isLower, isUpper, isAlpha)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Regex Tests" [
    characterClassTests,
    regexEvalTests,
    parserTests
  ]

characterClassTests :: TestTree
characterClassTests = testGroup "Character Class Tests"
  [ testCase "SingleChar matches correct character" $ do
      matchCharClass (SingleChar 'a') 'a' @?= True
      matchCharClass (SingleChar 'a') 'b' @?= False

  , testCase "CharRange matches characters in range" $ do
      matchCharClass (CharRange 'a' 'z') 'c' @?= True
      matchCharClass (CharRange 'a' 'z') '5' @?= False
      matchCharClass (CharRange '0' '9') '5' @?= True

  , testCase "Digit matches only digits" $ do
      matchCharClass Digit '5' @?= True
      matchCharClass Digit 'a' @?= False
      all (matchCharClass Digit) "0123456789" @? "Should match all digits"

  , testCase "Alpha matches only letters" $ do
      matchCharClass Alpha 'a' @?= True
      matchCharClass Alpha 'Z' @?= True
      matchCharClass Alpha '5' @?= False

  , testCase "Lower matches only lowercase" $ do
      matchCharClass Lower 'a' @?= True
      matchCharClass Lower 'Z' @?= False
      all (matchCharClass Lower) "abcdefghijklmnopqrstuvwxyz" @? "Should match all lowercase"

  , testCase "Upper matches only uppercase" $ do
      matchCharClass Upper 'A' @?= True
      matchCharClass Upper 'a' @?= False
      all (matchCharClass Upper) "ABCDEFGHIJKLMNOPQRSTUVWXYZ" @? "Should match all uppercase"
  ]

regexEvalTests :: TestTree
regexEvalTests = testGroup "Regex Evaluation Tests"
  [ testCase "Literal matches single character" $ do
      eval (Literal 'a') "a" @?= True
      eval (Literal 'a') "b" @?= False
      eval (Literal 'a') "" @?= False
      eval (Literal 'a') "aa" @?= False

  , testCase "Concatenation matches sequence" $ do
      let regex = Concat (Literal 'a') (Literal 'b')
      eval regex "ab" @?= True
      eval regex "ba" @?= False
      eval regex "a" @?= False
      eval regex "abc" @?= False

  , testCase "Star matches zero or more" $ do
      let regex = Star (Literal 'a')
      eval regex "" @?= True
      eval regex "a" @?= True
      eval regex "aaa" @?= True
      eval regex "b" @?= False

  , testCase "Plus matches one or more" $ do
      let regex = Plus (Literal 'a')
      eval regex "" @?= False
      eval regex "a" @?= True
      eval regex "aaa" @?= True
      eval regex "b" @?= False

  , testCase "Question matches zero or one" $ do
      let regex = Question (Literal 'a')
      eval regex "" @?= True
      eval regex "a" @?= True
      eval regex "aa" @?= False

  , testCase "Disjunction matches either pattern" $ do
      let regex = Disjunction (Literal 'a') (Literal 'b')
      eval regex "a" @?= True
      eval regex "b" @?= True
      eval regex "c" @?= False
      eval regex "ab" @?= False

  , testCase "Complex pattern matching" $ do
      let regex = Concat (Plus (Literal 'a')) (Star (Literal 'b'))
      eval regex "a" @?= True
      eval regex "aaa" @?= True
      eval regex "ab" @?= True
      eval regex "aabb" @?= True
      eval regex "b" @?= False
      eval regex "" @?= False
  ]

parserTests :: TestTree
parserTests = testGroup "Parser Tests"
  [ testCase "Parse literal" $
      parse "a" @?= Right (Literal 'a')

  , testCase "Parse concatenation" $
      parse "ab" @?= Right (Concat (Literal 'a') (Literal 'b'))

  , testCase "Parse star" $
      parse "a*" @?= Right (Star (Literal 'a'))

  , testCase "Parse plus" $
      parse "a+" @?= Right (Plus (Literal 'a'))

  , testCase "Parse question" $
      parse "a?" @?= Right (Question (Literal 'a'))

  , testCase "Parse character class" $
      parse "[a-z]" @?= Right (CharClassMatch (CharRange 'a' 'z'))

  , testCase "Parse disjunction" $
      parse "a|b" @?= Right (Disjunction (Literal 'a') (Literal 'b'))

  , testCase "Parse complex expression" $
      parse "(a|b)*c+" @?= Right (Concat 
          (Star (Disjunction (Literal 'a') (Literal 'b')))
          (Plus (Literal 'c')))

  , testCase "Parse empty string" $
      case parse "" of
        Left EmptyRegex -> return ()
        _ -> assertFailure "Should return EmptyRegex error"

  , testCase "Parse invalid range" $
      case parse "[z-a]" of
        Left (InvalidRange 'z' 'a') -> return ()
        _ -> assertFailure "Should return InvalidRange error"

  , testCase "Parse invalid escape" $
      case parse "\\x" of
        Left (InvalidEscape "x") -> return ()
        _ -> assertFailure "Should return InvalidEscape error"

  , testCase "Parse special character classes" $ do
      parse "\\d" @?= Right (CharClassMatch Digit)
      parse "\\a" @?= Right (CharClassMatch Alpha)
      parse "\\l" @?= Right (CharClassMatch Lower)
      parse "\\u" @?= Right (CharClassMatch Upper)
  ]