module Test.Compiler (props) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import HW.Compiler
import HW.StackMachine
import HW.Eval

import qualified Expr as E

import Test.Tasty
import Test.Tasty.Hedgehog

-- Generator for numeric expressions
genNumExpr :: Gen (E.Expr String)
genNumExpr = E.Num <$> Gen.int (Range.linear (-1000) 1000)

-- Generator for addition expressions
genAddExpr :: Gen (E.Expr String)
genAddExpr = do
  left <- genNumExpr
  right <- genNumExpr
  return $ E.Plus left right

-- Property 1: Compiling and executing a numeric expression results in the correct value
prop_compileNumExpr :: Property
prop_compileNumExpr = property $ do
  expr <- forAll genNumExpr
  let program = compile expr
  let result = execProgram program initialState
  case result of
    Right finalState -> do
      let stack = getStack finalState
      stack === [extractNum expr]
    Left err -> do
      footnote $ "Execution failed with error: " ++ show err
      failure

-- Helper to extract the number from a Num expression
extractNum :: E.Expr v -> Int
extractNum (E.Num n) = n
extractNum _ = error "Expected Num expression"

-- Property 2: Compiling and executing an addition expression results in the correct sum
prop_compileAddExpr :: Property
prop_compileAddExpr = property $ do
  expr <- forAll genAddExpr
  let program = compile expr
  let result = execProgram program initialState
  case result of
    Right finalState -> do
      let stack = getStack finalState
      stack === [evalExpr expr]
    Left err -> do
      footnote $ "Execution failed with error: " ++ show err
      failure

-- Helper to evaluate the expression
evalExpr :: E.Expr v -> Int
evalExpr (E.Num n) = n
evalExpr (E.Plus l r) = evalExpr l + evalExpr r
evalExpr _ = error "Expected numeric expression"

-- Property 3: Compiled program is non-empty for valid expressions
prop_compileProducesProgram :: Property
prop_compileProducesProgram = property $ do
  expr <- forAll $ Gen.choice [genNumExpr, genAddExpr]
  let program = compile expr
  assert (not (null program))

props :: [TestTree]
props =
  [ testProperty "Compiling Num expression results in correct value" prop_compileNumExpr
  , testProperty "Compiling Plus expression results in correct sum" prop_compileAddExpr
  , testProperty "Compiled program is non-empty for valid expressions" prop_compileProducesProgram
  ]