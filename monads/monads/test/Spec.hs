import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Data.Map as M

import HW.Compiler
import HW.StackMachine
import Expr
import HW.Eval
import State.MyState

genInt :: Gen Int
genInt = Gen.int (Range.constant 0 100)

genVar :: Gen String
genVar = Gen.element ["x", "y", "z", "a", "b"]

genNumExpr :: Gen (Expr String)
genNumExpr = Expr.Num <$> genInt

genVarExpr :: Gen (Expr String)
genVarExpr = Expr.Var <$> genVar

genAddExpr :: Gen (Expr String)
genAddExpr = Expr.Plus <$> genExpr <*> genExpr

genLetExpr :: Gen (Expr String)
genLetExpr = Let <$> genVar <*> genExpr <*> genExpr

genExpr :: Gen (Expr String)
genExpr = Gen.recursive Gen.choice
    [genNumExpr, genVarExpr]
    [genAddExpr, genLetExpr]

-- Checks for the proper execution of a stack program
properExecution :: Property
properExecution = property $ do
    expr <- forAll (genExpr)
    let program = compile expr
    let initial = MachineState [] (M.fromList [("x", 10), ("y", 20), ("z", 30), ("a", 8), ("b", 33)]) 
    let result = execProgram program initial
    case result of
        Right executed -> do
            let stack = getStack executed
            case stack of
                [one] -> success
                _ -> fail "Expected one result on the stack"
        Left err -> fail $ "Program execution unsuccessful: " ++ show err

-- Detection of StackNotExhausted Error
stackExhaustionError :: Property
stackExhaustionError = property $ do
    expr <- forAll (genExpr)
    let program = compile expr ++ [PushNum 0]
    let initial = MachineState [] (M.fromList [("x", 10), ("y", 20), ("z", 30), ("a", 8), ("b", 33)])
    let result = execProgram program initial
    case result of
        Left (StackNotExhausted _) -> success
        _ -> fail "StackNotExhausted Error Undetected"

-- Detection of StackUnderflow Error
stackUnderflowError :: Property
stackUnderflowError = property $ do
    expr <- forAll (genExpr)
    let program = compile expr ++ [Add]
    let initial = MachineState [] (M.fromList [("x", 10), ("y", 20), ("z", 30), ("a", 8), ("b", 33)])
    let result = execProgram program initial
    case result of
        Left (StackUnderflow _) -> success
        _ -> fail "StackUnderflow Error Undetected"

-- Undefined var error
undefinedVarError :: Property
undefinedVarError = property $ do
    x <- forAll (genVarExpr)
    let program = compile x
    let initial = MachineState [] M.empty
    let result = execProgram program initial
    case result of
        Left (VarUndefined _) -> success
        _ -> fail "Undefined Var Error Undetected"

-- Checking for var shadowing
letShadowing :: Property
letShadowing = property $ do
    x <- forAll (genVar)
    num1 <- forAll (genInt)
    num2 <- forAll (genInt)
    let expr = Let x (Expr.Num num1) (Let x (Plus (Expr.Num num1) (Expr.Num num2)) (Expr.Var x))
    let program = compile expr
    let initial = MachineState [] M.empty
    let result = execProgram program initial
    case result of
        Right executed -> do
            let stack = getStack executed
            let expectedStack = [num1 + num2]
            assert (stack == expectedStack)
        Left err -> fail $ "Execution failed: " ++ show err

-- List of properties to test
props :: [TestTree]
props =
  [ testProperty "Proper execution of stack program" properExecution
  , testProperty "Detection of StackNotExhausted Error" stackExhaustionError
  , testProperty "Detection of StackUnderflow Error" stackUnderflowError
  , testProperty "Undefined variable error detection" undefinedVarError
  , testProperty "Variable shadowing in Let bindings" letShadowing
  ]

main :: IO ()
main = defaultMain $ testGroup "Expression Language" props