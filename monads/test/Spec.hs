module Main (main) where

import qualified Data.Map as M

import HW.StackMachine
import HW.Compiler
import HW.Eval
import Expr

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit
import Test.Tasty


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ compilerTests
  , evalTests
  , propTests
  ]

compilerTests :: TestTree
compilerTests = testGroup "Compiler Tests"
  [ testCase "Compile single number" $
      [PushNum 5] @?= (compile (Num 5 :: Expr String))

  , testCase "Compile single variable" $
      [PushVar "x"] @?= (compile (Expr.Var "x" :: Expr String))

  , testCase "Compile addition" $
      [PushNum 5, PushNum 10, Add] @?= (compile (Plus (Num 5) (Num 10) :: Expr String))

  , testCase "Compile let expression" $
        [PushNum 5, StoreVar "x", PushVar "x", PushNum 2, Add] @?=
        (compile (Let "x" (Num 5) (Plus (Expr.Var "x") (Num 2)) :: Expr String))

  , testCase "Compile complex let expression" $
        [PushNum 13, StoreVar "x", PushNum 42, StoreVar "y", PushVar "x", PushVar "y", Add, StoreVar "x", PushVar "x", PushVar "y", Add] @?=
        (compile (Let "x" (Num 13) (Let "y" (Num 42) (Let "x" (Plus (Expr.Var "x") (Expr.Var "y")) (Plus (Expr.Var "x") (Expr.Var "y")))) :: Expr String))
  ]

evalTests :: TestTree
evalTests = testGroup "Evaluation Tests"
  [testCase "Evaluate a correct program" $
      (Right (MachineState [17] (M.singleton "x" 15))) @?= (execProgram exampleProgram initialState)

  , testCase "Stack underflow during addition" $
      (Left (StackUnderflow Add)) @?= (execProgram underflowExpr initialState)

  , testCase "Undefined variable" $
      (Left (VarUndefined "\"x\"")) @?= (execProgram undefVar initialState)

  , testCase "Stack not exhausted" $
      (Left (StackNotExhausted [10, 5])) @?= (execProgram [PushNum 5, PushNum 10] initialState)

  , testCase "Store variable" $
      (Right (MachineState [7] (M.singleton "x" 5))) @?= (execProgram [PushNum 5, StoreVar "x", PushVar "x", PushNum 2, Add] initialState)

  , testCase "Store more variables" $
        (Right (MachineState [10] (M.fromList [("x", 5), ("y", 5)]))) @?=
        (execProgram
          [PushNum 5, StoreVar "x",
           PushNum 5, StoreVar "y",
           PushVar "x", PushVar "y", Add]
          initialState)
  ]

genNum :: Gen Int
genNum = Gen.int (Range.constant (-1000) 1000)

genVar :: Gen String
genVar = Gen.element ["x", "y", "z"]

genNumExpr :: Gen (Expr String)
genNumExpr = Expr.Num <$> genNum

genVarExpr :: Gen (Expr String)
genVarExpr = Expr.Var <$> genVar

genAdd :: Gen (Expr String)
genAdd = Expr.Plus <$> genExpr <*> genExpr

genExpr :: Gen (Expr String)
genExpr = Gen.recursive Gen.choice [genNumExpr, genVarExpr] [genAdd]


--------------------------------------------------------- HW07 ---------------------------------------------------------

-- Define evalDirect to evaluate expressions given an environment
evalDirect :: Expr String -> M.Map String Int -> Either (Error String) Int
evalDirect (Expr.Num n) _ = Right n
evalDirect (Expr.Var v) env =
  case M.lookup v env of
    Just val -> Right val
    Nothing  -> Left (VarUndefined v)
evalDirect (Expr.Plus e1 e2) env = do
  val1 <- evalDirect e1 env
  val2 <- evalDirect e2 env
  return (val1 + val2)

propTests :: TestTree
propTests = testGroup "Property Tests"
  [ testProperty "Evaluated program matches direct evaluation"              prop_directEvaluationConsistency
  , testProperty "StoreVar properly saves and retrieves variable"           prop_storeVarConsistency
  , testProperty "Multiple Let bindings respect scoping rules"              prop_letScopeNesting
  , testProperty "Expression with only numbers compiles without variables"  prop_compileNoVariables
  , testProperty "Compilation preserves number of constants"                prop_preserveNumConstants
  , testProperty "Addition with zero leaves the value unchanged"            prop_addZero
  , testProperty "Compilation of nested Plus expressions is associative"    prop_plusAssociativity
  , testProperty "Running empty program leaves stack unchanged"             prop_emptyProgramNoChange
  , testProperty "Shadowing with Let bindings keeps inner value"            prop_shadowingConsistency
  ]

-- Check that the result of executing a program matches the direct evaluation of the expression
prop_directEvaluationConsistency :: Property
prop_directEvaluationConsistency = property $ do
    expr <- forAll genExpr
    let result = execProgram (compile expr) (MachineState [] M.empty)
    case (result, evalDirect expr M.empty) of
        (Right (MachineState [val] _), Right expected) -> val === expected
        (Left _, Left _) -> success
        _ -> fail "Direct evaluation and compiled execution mismatch"

-- Check that StoreVar correctly saves a variable and allows it to be retrieved
prop_storeVarConsistency :: Property
prop_storeVarConsistency = property $ do
    varName <- forAll genVar
    val <- forAll genNum
    let program = [PushNum val, StoreVar varName, PushVar varName]
    let result = execProgram program (MachineState [] M.empty)
    case result of
        Right (MachineState [resultVal] _) -> resultVal === val
        _ -> fail "Stored value could not be retrieved correctly"

-- Check that multiple Let bindings respect scoping rules
prop_letScopeNesting :: Property
prop_letScopeNesting = property $ do
    var1 <- forAll genVar
    var2 <- forAll (Gen.filter (/= var1) genVar)
    val1 <- forAll genNum
    val2 <- forAll genNum
    let expr = Let var1 (Expr.Num val1) (Let var2 (Expr.Num val2) (Plus (Expr.Var var1) (Expr.Var var2)))
    let result = execProgram (compile expr) (MachineState [] M.empty)
    case result of
        Right (MachineState [resultVal] _) -> resultVal === (val1 + val2)
        _ -> fail "Nested Let bindings did not evaluate correctly"

-- Check that compiling an expression with only numbers does not create variables
prop_compileNoVariables :: Property
prop_compileNoVariables = property $ do
    expr <- forAll genNumExpr
    Hedgehog.assert $ all (\x -> case x of { PushVar _ -> False; _ -> True }) (compile expr)

-- Check that compilation preserves the number of numeric constants
prop_preserveNumConstants :: Property
prop_preserveNumConstants = property $ do
    expr <- forAll genExpr
    countConstants expr === length (filter isPushNum (compile expr))
  where
    countConstants (Expr.Num _) = 1
    countConstants (Expr.Var _) = 0
    countConstants (Plus a b)    = countConstants a + countConstants b
    countConstants (Let _ e1 e2) = countConstants e1 + countConstants e2
    isPushNum (PushNum _) = True
    isPushNum _           = False

-- Check that adding zero does not change the value
prop_addZero :: Property
prop_addZero = property $ do
    num <- forAll genNum
    let expr = Plus (Num num) (Num 0)
    let result = execProgram (compile expr) (MachineState [] (M.empty :: M.Map String Int))
    case result of
        Right (MachineState [val] _) -> val === num
        _ -> fail "Addition with zero did not yield original value"

-- Check that compilation of nested Plus expressions is associative
prop_plusAssociativity :: Property
prop_plusAssociativity = property $ do
    x <- forAll genNum
    y <- forAll genNum
    z <- forAll genNum
    let expr1 = Plus (Num x) (Plus (Num y) (Num z))
    let expr2 = Plus (Plus (Num x) (Num y)) (Num z)
    let result1 = execProgram (compile expr1) (MachineState [] (M.empty :: M.Map String Int))
    let result2 = execProgram (compile expr2) (MachineState [] (M.empty :: M.Map String Int))
    result1 === result2

-- Check that executing an empty program does not change the stack
prop_emptyProgramNoChange :: Property
prop_emptyProgramNoChange = property $ do
    let initial = MachineState [1, 2, 3] (M.empty :: M.Map String Int)
    let result = execProgram [] initial
    case result of
        Left (StackNotExhausted stack) -> stack === [1, 2, 3]
        _ -> fail "Expected StackNotExhausted error"

-- Check that Let with variable shadowing correctly uses inner variable value
prop_shadowingConsistency :: Property
prop_shadowingConsistency = property $ do
    x <- forAll genVar
    numOuter <- forAll genNum
    numInner <- forAll genNum
    let expr = Let x (Num numOuter) (Let x (Num numInner) (Expr.Var x))
    let result = execProgram (compile expr) (MachineState [] M.empty)
    case result of
        Right (MachineState [val] _) -> val === numInner
        _ -> fail "Let shadowing did not use inner variable value"
