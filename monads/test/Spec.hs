import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map.Strict as M
import qualified HW.Eval
import qualified Expr
import qualified HW.Compiler
import qualified HW.StackMachine
import State.MyState (runMyState)


-- Test suite for compile function
testCompile :: TestTree
testCompile = testGroup "compile tests"
  [ testCase "compile simple let expression" $
      let expr = Expr.Let "x" (Expr.Num 5) (Expr.Plus (Expr.Var "x") (Expr.Num 10))
          expected = [ HW.StackMachine.PushNum 5, HW.StackMachine.StoreVar "x", HW.StackMachine.PushVar "x", HW.StackMachine.PushNum 10, HW.StackMachine.Add]
      in HW.Compiler.compile expr @?= expected

  , testCase "compile nested let expression" $
      let expr = Expr.Let "x" (Expr.Num 13)
                    (Expr.Let "y" (Expr.Num 42)
                      (Expr.Let "x" (Expr.Plus (Expr.Var "x") (Expr.Var "y"))
                        (Expr.Plus (Expr.Var "x") (Expr.Var "y"))))
          expected = [HW.StackMachine.PushNum 13, HW.StackMachine.StoreVar "x", 
                      HW.StackMachine.PushNum 42, HW.StackMachine.StoreVar "y", 
                      HW.StackMachine.PushVar "x", HW.StackMachine.PushVar "y", HW.StackMachine.Add, HW.StackMachine.StoreVar "x", 
                      HW.StackMachine.PushVar "x", HW.StackMachine.PushVar "y", HW.StackMachine.Add]
      in HW.Compiler.compile expr @?= expected
  ]

-- Test suite for execInstr function
testExecInstr :: TestTree
testExecInstr = testGroup "execInstr tests"
  [ testCase "PushNum 5" $ do
      let (finalState1, results1) = runMyState (HW.Eval.execInstr (HW.StackMachine.PushNum 5)) HW.Eval.initialState
      assertEqual "Stack should contain 5" (Right ()) results1
      assertEqual "Final stack should be [5]" [5] (HW.Eval.getStack finalState1)

  , testCase "Add should fail due to stack underflow" $ do
      let (finalState2, results2) = runMyState (HW.Eval.execInstr HW.StackMachine.Add) HW.Eval.initialState
      assertEqual "Add should fail due to stack underflow" (Left (HW.Eval.StackUnderflow HW.StackMachine.Add)) results2

  , testCase "PushVar undefined" $ do
      let (finalState3, results3) = runMyState (HW.Eval.execInstr (HW.StackMachine.PushVar "x")) HW.Eval.initialState
      assertEqual "PushVar should fail due to undefined variable" (Left (HW.Eval.VarUndefined "x")) results3

  , testCase "StoreVar with empty stack" $ do
      let (finalState4, results4) = runMyState (HW.Eval.execInstr (HW.StackMachine.StoreVar "x")) HW.Eval.initialState
      assertEqual "StoreVar should fail due to stack underflow" (Left (HW.Eval.StackUnderflow (HW.StackMachine.StoreVar "x"))) results4

  , testCase "Add with insufficient values on the stack" $ do
      let (finalState5, results5) = runMyState (HW.Eval.execInstr HW.StackMachine.Add) HW.Eval.initialState
      assertEqual "Add should fail due to stack underflow" (Left (HW.Eval.StackUnderflow HW.StackMachine.Add)) results5
  ]


-- Test suite for execProgram function
testExecProgram :: TestTree
testExecProgram = testGroup "execProgram tests"
  [ testCase "run exampleProgram" $
      let program = [HW.StackMachine.PushNum 5, HW.StackMachine.PushNum 10, HW.StackMachine.Add, HW.StackMachine.StoreVar "x", HW.StackMachine.PushVar "x", HW.StackMachine.PushNum 2, HW.StackMachine.Add]
      in HW.Eval.execProgram program HW.Eval.initialState @?= Right (HW.Eval.MachineState [17] (M.fromList [("x", 15)]))

  , testCase "run underflowExpr" $
      let program = [HW.StackMachine.PushNum 13, HW.StackMachine.Add]
      in HW.Eval.execProgram program HW.Eval.initialState @?= Left (HW.Eval.StackUnderflow HW.StackMachine.Add)

  , testCase "run undefVar program" $
      let program = [HW.StackMachine.PushVar "x"]
      in HW.Eval.execProgram program HW.Eval.initialState @?= Left (HW.Eval.VarUndefined "x")

  , testCase "complex nested Expr.let program" $
      let program = HW.Compiler.compile (Expr.Let "x" (Expr.Num 13)
                              (Expr.Let "y" (Expr.Num 42)
                                (Expr.Let "x" (Expr.Plus (Expr.Var "x") (Expr.Var "y"))
                                  (Expr.Plus (Expr.Var "x") (Expr.Var "y")))))
          expected = Right (HW.Eval.MachineState [97] (M.fromList [("x", 55), ("y", 42)]))
      in HW.Eval.execProgram program HW.Eval.initialState @?= expected
  ]


main :: IO ()
main = defaultMain $ testGroup "Stack Machine Tests"
  [ testCompile
  , testExecInstr
  , testExecProgram
  ]

