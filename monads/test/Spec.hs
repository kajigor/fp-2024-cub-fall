{-# LANGUAGE OverloadedStrings #-}

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Data.Map as M

import HW.StackMachine
import HW.Eval
import HW.Compiler
import Reader.Eval
import Reader.MyReader
import Expr

-- Helper function to evaluate an expression with a custom context
evalWithContext :: (Ord a) => Expr a -> M.Map a Int -> Int
evalWithContext expr context = Reader.MyReader.runMyReader (Reader.Eval.eval expr) context

-- Generate an integer within a range
genInt :: Gen Int
genInt = Gen.int (Range.constant 0 100)

-- Generate expressions using only predefined variables from the initial context
genExpr :: [String] -> Gen (Expr String)
genExpr boundVars = Gen.recursive Gen.choice baseGens recursiveGens
  where
    baseGens :: [Gen (Expr String)]
    baseGens =
      [ Num <$> genInt ] ++
      [ Expr.Var <$> Gen.element boundVars | not (null boundVars) ]

    recursiveGens :: [Gen (Expr String)]
    recursiveGens =
      [ Plus <$> genExpr boundVars <*> genExpr boundVars ] ++
      [ genLetExpr | not (null availableVars) ]
      where
        varNames = map (:[]) ['a'..'z']
        availableVars = filter (`notElem` boundVars) varNames
        genLetExpr = do
          var <- Gen.element availableVars
          e1 <- genExpr boundVars
          e2 <- genExpr (var : boundVars)
          return (Let var e1 e2)


-- Property to test that compiled expressions evaluate correctly with a fixed context
prop_compile_correct :: Property
prop_compile_correct = property $ do
  expr <- forAll $ genExpr []
  let expected = evalWithContext expr M.empty
  let compiled = compile expr
  case execProgram compiled initialState of
    Right finalState ->
      case getStack finalState of
        (result:_) -> assert (result == expected)
        _ -> fail "Stack was empty after execution"
    Left err -> fail $ "prop_compile_correct failed: " ++ show err

prop_compile_varUndefined :: Property
prop_compile_varUndefined = property $ do
  expr <- forAll $ genExpr []
  let var_str = "ç"
  let compiled = compile expr ++ [PushVar var_str]
  case execProgram compiled initialState of
    Left (VarUndefined variable) -> assert (variable == var_str)
    _ -> fail "Expected VarUndefined error"

prop_compile_stackUnderflow :: Property
prop_compile_stackUnderflow = property $ do
  expr <- forAll $ genExpr []
  let compiled = compile expr ++ [Add]
  case execProgram compiled initialState of
    Left (StackUnderflow _) -> success
    _ -> fail "Expected StackUnderflow error"

prop_compile_stackNotExhausted :: Property
prop_compile_stackNotExhausted = property $ do
  int1 <- forAll genInt
  int2 <- forAll genInt
  expr <- forAll $ genExpr []
  let compiled = compile expr ++ [PushNum int1, PushNum int2]
  case execProgram compiled initialState of
    Left (StackNotExhausted _) -> success
    Right _ -> fail "Expected StackNotExhausted error due to extra items on the stack"
    Left err -> fail $ "Unexpected error: " ++ show err

tests :: [TestTree]
tests =
  [ testProperty "Compiled program evaluates correctly" prop_compile_correct,
    testProperty "Compiled program fails with varUndefined error" prop_compile_varUndefined,
    testProperty "Compiled program fails with stackUnderflow error" prop_compile_stackUnderflow,
    testProperty "Compiled program fails with stackNotExhausted error" prop_compile_stackNotExhausted
  ]

-- Main function to run the tests
main :: IO ()
main = defaultMain (testGroup "Tests" tests)