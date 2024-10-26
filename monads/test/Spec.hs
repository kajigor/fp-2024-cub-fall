import qualified Data.Map.Strict as M

import Expr
import HW.Compiler
import HW.StackMachine
import HW.Eval

import Test.Tasty
import Test.Tasty.HUnit

testCompiler :: TestTree
testCompiler =
  testGroup "Compiler" [testExpressions, testComplexExpressions]
  where
    testExpressions =
      testGroup
        "Test every expression type"
        [ testCompilerSuccess "Num" (Num 3 :: Expr String) [PushNum 3],
          testCompilerSuccess "Var" (Var "x" :: Expr String) [PushVar "x"],
          testCompilerSuccess "Plus" (Plus (Num 1) (Num 2) :: Expr String) [PushNum 1, PushNum 2, Add],
          testCompilerSuccess "Let" (Let "foo" (Num 1) (Plus (Var "foo") (Num 2)) :: Expr String) [PushNum 1, StoreVar "foo", PushVar "foo", PushNum 2, Add]
        ]
    testComplexExpressions =
      testGroup
        "Test complex expressions"
        [ testCompilerSuccess "Let with nested Plus"
            (Let "x" (Plus (Num 1) (Num 2)) (Plus (Var "x") (Num 3)))
            [PushNum 1, PushNum 2, Add, StoreVar "x", PushVar "x", PushNum 3, Add],
          testCompilerSuccess "Nested Lets"
            (Let "x" (Num 1) (Let "y" (Num 2) (Plus (Var "x") (Var "y"))))
            [PushNum 1, StoreVar "x", PushNum 2, StoreVar "y", PushVar "x", PushVar "y", Add],
          testCompilerSuccess "Let with Var in body"
            (Let "x" (Num 1) (Var "x"))
            [PushNum 1, StoreVar "x", PushVar "x"]
        ]

    testCompilerSuccess msg expr expected =
      testCase msg $
        let res = compile expr in
          assertEqual ("Compilation result is wrong, should be \n" ++ (show expected) ++ "\bbut got\n" ++ (show res) ++ "\n") res expected

testExecutor :: TestTree
testExecutor = 
  testGroup "Executor" [testIncluded, testCustom]
  where 
    testIncluded = 
      testGroup "Test included programs"
      [
        testExecutorSuccess "Example program" exampleProgram initialState (MachineState [17] (M.singleton "x" 15)),
        testExecutorFail "Stack underflow" underflowExpr initialState (StackUnderflow Add),
        testExecutorFail "Variable undefined" undefVar initialState (VarUndefined "\"x\"")
      ]
    testCustom = 
      testGroup "Test custom programs"
      [
        testExecutorFail "Stack not exhausted" [PushNum 1, PushNum 2] initialState (StackNotExhausted [2, 1])
      ]
    testExecutorSuccess msg program state expected = 
      testCase msg $
        let res = execProgram program state in
          assertEqual ("Executor result is wrong, should be \n" ++ (show expected) ++ "\bbut got\n" ++ (show res) ++ "\n") res (Right expected)

    testExecutorFail msg program state expected = 
      testCase msg $
        let res = execProgram program state in
          assertEqual ("Executor result is wrong, should be \n" ++ (show expected) ++ "\bbut got\n" ++ (show res) ++ "\n") res (Left expected)
main :: IO ()
main = defaultMain $ testGroup "Main tests" [testCompiler, testExecutor]