{-# LANGUAGE OverloadedStrings #-}

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)

import qualified Data.Map as M

import HW.StackMachine
import HW.Eval (execProgram, initialState, Error (VarUndefined, StackUnderflow, StackNotExhausted))
import HW.Compiler
import Expr

-- Generate random integers for testing
genInt :: Gen Int
genInt = Gen.int (Range.constant 0 100)

-- Generate random variable names
genVarName :: Gen String
genVarName = Gen.element ["x", "y", "z", "a", "b", "c"]

-- Generate expressions that can be used in property testing
genExpr :: Int -> [String] -> Gen (Expr String)
genExpr 0 boundVars =
  if null boundVars
    then Num <$> genInt
    else Gen.choice [Num <$> genInt, Expr.Var <$> Gen.element boundVars]
genExpr n boundVars = do
  let newVars = ["x", "y", "z"]
  let availableVars = filter (`notElem` boundVars) newVars
  let varGen = if null boundVars then [] else [Expr.Var <$> Gen.element boundVars]
  let letGen = if null availableVars then [] else
        [do
          var <- Gen.element availableVars
          e1 <- genExpr (n - 1) boundVars
          e2 <- genExpr (n - 1) (var : boundVars)
          return (Let var e1 e2)]
  Gen.choice $
    [Num <$> genInt]
    ++ varGen
    ++ [Plus <$> genExpr (n - 1) boundVars <*> genExpr (n - 1) boundVars]
    ++ letGen

-- Property: Ensure that compiled expressions execute without runtime errors
prop_compilerExecutes :: Property
prop_compilerExecutes = property $ do
  expr <- forAll $ genExpr 5 []
  let program = compile expr
  case execProgram program initialState of
    Right _ -> success
    Left _ -> failure

-- Property: Undefined variable error is handled correctly
prop_undefinedVariableError :: Property
prop_undefinedVariableError = property $ do
  let program = [PushVar "undefined"]
  case execProgram program initialState of
    Left (VarUndefined var) | var == "undefined" -> success
    _ -> failure

-- Property: Stack underflow error is handled correctly
prop_stackUnderflowError :: Property
prop_stackUnderflowError = property $ do
  let program = [Add]
  case execProgram program initialState of
    Left (StackUnderflow _) -> success
    _ -> failure

-- List of properties
tests :: [TestTree]
tests =
  [ testProperty "Compiled expressions execute without errors" prop_compilerExecutes
  , testProperty "Undefined variable error is raised correctly" prop_undefinedVariableError
  , testProperty "Stack underflow error is raised correctly" prop_stackUnderflowError
  ]

-- Main function to run all properties
main :: IO ()
main = defaultMain $ testGroup "Homework Tests" tests
