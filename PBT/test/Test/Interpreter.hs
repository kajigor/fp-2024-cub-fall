module Test.Interpreter (props) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import HW.Eval
import HW.StackMachine

import Test.Tasty
import Test.Tasty.Hedgehog

import Data.Either (isRight, isLeft)

-- Property 1: Executing PushNum and Add instructions computes the correct sum
prop_addition :: Property
prop_addition = property $ do
  nums <- forAll $ Gen.list (Range.linear 1 10) (Gen.int (Range.linear (-1000) 1000))
  let program = map PushNum nums ++ replicate (length nums - 1) Add
  let expectedSum = sum nums
  let result = execProgram program initialState
  case result of
    Right finalState -> do
      let stack = getStack finalState
      stack === [expectedSum]
    Left err -> do
      footnote $ "Execution failed with error: " ++ show err
      failure

-- Property 2: Executing Add with insufficient operands results in StackUnderflow
prop_stackUnderflow :: Property
prop_stackUnderflow = property $ do
  numPushes <- forAll $ Gen.int (Range.linear 0 1) -- 0 or 1 PushNum instructions
  nums <- forAll $ Gen.list (Range.singleton numPushes) (Gen.int (Range.linear (-1000) 1000))
  let program = map PushNum nums ++ [Add]
  let result = execProgram program initialState
  assert (case result of
            Left (StackUnderflow _) -> True
            _ -> False)

-- Property 3: Stack size increases by one after PushNum
prop_pushNumIncreasesStack :: Property
prop_pushNumIncreasesStack = property $ do
  num <- forAll $ Gen.int (Range.linear (-1000) 1000)
  let program = [PushNum num]
  let result = execProgram program initialState
  case result of
    Right finalState -> do
      let stack = getStack finalState
      length stack === 1
    Left err -> do
      footnote $ "Execution failed with error: " ++ show err
      failure

props :: [TestTree]
props =
  [ testProperty "Addition computes correct sum" prop_addition
  , testProperty "Add with insufficient operands causes StackUnderflow" prop_stackUnderflow
  , testProperty "PushNum increases stack size by one" prop_pushNumIncreasesStack
  ]