module Tests.Spellcheck (testSpellcheck) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, (@?=))
import Dictionary
import Spellcheck

testSpellcheck :: TestTree
testSpellcheck =
  testGroup "Spellcheck" [testEdits1OneLetter, testEdits1TwoLetters, testCandidatesSimple, testCandidatesDistance2]
  where
    -- 0 deletes + 0 transposes + 25 replaces + 26 * 2 inserts = 77
    testEdits1OneLetter = testCase "edits1: 1 letter" $ (length $ edits1 "a") @?= 77
    -- 2 deletes + 1 transpose + 25 * 2 replaces + 26 * 3 inserts - 2 duplicates = 129
    testEdits1TwoLetters = testCase "edits1: 2 letters" $ (length $ edits1 "ab") @?= 129
    testCandidatesSimple = testCase "candidates: simple" $ do
      let dict = Dictionary.fromList ["hello", "world"]
      (candidates dict "helo") @?= Just ["hello"]
    testCandidatesDistance2 = testCase "candidates: distance 2" $ do
      let dict = Dictionary.fromList ["hello", "world"]
      (candidates dict "helo") @?= Just ["hello"]
    testCandidatesRanking = testCase "candidates: ranking" $ do
      let dict = Dictionary.fromList ["hello", "he$$o", "meow", "hello", "meow", "meow"]
      (candidates dict "heo") @?= Just ["meow", "hello", "he$$o"]

