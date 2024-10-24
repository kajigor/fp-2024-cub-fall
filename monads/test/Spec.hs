
import Test.Tasty
import Test.Tasty.HUnit
import HW.Compiler
import HW.StackMachine
import HW.Eval
import Expr (Expr(..)) 

expr1 :: Expr String
expr1 = Let "x" (Num 13) (Plus (Var "x") (Num 42))

compiledExpr1 :: StackProgram String
compiledExpr1 = [PushNum 13, StoreVar "x", PushVar "x", PushNum 42, Add]

expr2 :: Expr String
expr2 = Let "x" (Num 5) (Let "y" (Plus (Var "x") (Num 3)) (Plus (Var "x") (Var "y")))

compiledExpr2 :: StackProgram String
compiledExpr2 = 
  [ PushNum 5, StoreVar "x"
  , PushVar "x", PushNum 3, Add, StoreVar "y"
  , PushVar "x", PushVar "y", Add
  ]

expr3 :: Expr String
expr3 = Plus (Var "x") (Var "y") 

expr4 :: Expr String
expr4 = Let "x" (Num 10) (Plus (Plus (Var "x") (Var "x")) (Var "x"))

compiledExpr4 :: StackProgram String
compiledExpr4 = [PushNum 10, StoreVar "x", PushVar "x", PushVar "x", Add, PushVar "x", Add]

expr5 :: StackProgram String
expr5 = [PushNum 1, PushNum 2] 

expr6 :: StackProgram String
expr6 = []

testCompilation :: TestTree
testCompilation = testGroup "Test compilation"
  [ testCase "let x = 13 in (x + 42)" $
      compile expr1 @?= compiledExpr1

  , testCase "let x = 5 in let y = (x + 3) in (x + y)" $
      compile expr2 @?= compiledExpr2

  , testCase "let x = 10 in (x + x + x)" $
      compile expr4 @?= compiledExpr4
  ]

testExecution :: TestTree
testExecution = testGroup "Test execution"
  [ testCase "Execution of let x = 13 in (x + 42)" $ do
      let result = execProgram (compile expr1) initialState
      case result of
        Right finalState -> getStack finalState @?= [55]
        Left err -> assertFailure $ "Execution failed with error: " ++ show err

  , testCase "Execution of let x = 5 in let y = (x + 3) in (x + y)" $ do
      let result = execProgram (compile expr2) initialState
      case result of
        Right finalState -> getStack finalState @?= [13]
        Left err -> assertFailure $ "Execution failed with error: " ++ show err

  , testCase "Execution of let x = 10 in (x + x + x)" $ do
      let result = execProgram (compile expr4) initialState
      case result of
        Right finalState -> getStack finalState @?= [30]
        Left err -> assertFailure $ "Execution failed with error: " ++ show err

  , testCase "Execution with multiple stack elements (StackNotExhausted)" $ do
      let result = execProgram expr5 initialState
      case result of
        Left (StackNotExhausted stack) -> stack @?= [2, 1]
        _ -> assertFailure "Expected StackNotExhausted error"

  , testCase "Execution with empty stack (StackNotExhausted)" $ do
      let result = execProgram expr6 initialState
      case result of
        Left (StackNotExhausted stack) -> stack @?= []
        _ -> assertFailure "Expected StackNotExhausted error"
  
  , testCase "Variable not defined (x + y)" $ do
      let result = execProgram (compile expr3) initialState
      case result of
        Left (VarUndefined var) -> var @?= "\"x\""
        _ -> assertFailure "Expected VarUndefined error"
  ]

tests :: TestTree
tests = testGroup "Compilation and Execution Tests"
  [ testCompilation
  , testExecution
  ]

main :: IO ()
main = defaultMain tests
