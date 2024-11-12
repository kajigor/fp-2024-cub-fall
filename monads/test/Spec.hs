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
genExpr :: Int -> [String] -> Gen (Expr String)
genExpr 0 boundVars =
  if null boundVars
    then Num <$> genInt
    else Gen.choice [Num <$> genInt, Expr.Var <$> Gen.element boundVars]

genExpr n boundVars = do
  let varNames = map (:[]) ['a'..'z']
  let availableVars = filter (`notElem` boundVars) varNames
  let varGen =
        if null boundVars
          then []
          else [Expr.Var <$> Gen.element boundVars]
  let letGen =
        if null availableVars
          then []
          else [do
                  var <- Gen.element availableVars
                  e1 <- genExpr (n - 1) boundVars
                  e2 <- genExpr (n - 1) (var : boundVars)
                  return (Let var e1 e2)
               ]
  Gen.choice $
    [Num <$> genInt]
    ++ varGen
    ++ [Plus <$> genExpr (n - 1) boundVars <*> genExpr (n - 1) boundVars]
    ++ letGen


-- Property to test that compiled expressions evaluate correctly with a fixed context
prop_compile_correct :: Property
prop_compile_correct = property $ do
  expr <- forAll $ genExpr 5 []
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
  var_name <- forAll $ Gen.string (Range.singleton 1) Gen.lower
  let expr :: Expr String
      expr = Expr.Var var_name
  let compiled = compile expr
  case execProgram compiled initialState of
    Left (VarUndefined variable) -> assert (variable == var_name)
    _ -> fail "Expected VarUndefined error"

prop_compile_stackUnderflow :: Property
prop_compile_stackUnderflow = property $ do
  let prog = [Add]
  case execProgram prog initialState of
    Left (StackUnderflow _) -> success
    _ -> fail "Expected StackUnderflow error"

prop_compile_stackNotExhausted :: Property
prop_compile_stackNotExhausted = property $ do
  int1 <- forAll genInt
  int2 <- forAll genInt
  let prog = [PushNum int1, PushNum int2]
  case execProgram prog initialState of
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