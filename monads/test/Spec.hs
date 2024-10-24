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
      let (finalState, result) = runMyState (HW.Eval.execInstr (HW.StackMachine.PushNum 5)) (HW.Eval.initialState :: HW.Eval.MachineState String)
      assertEqual "Stack should contain 5" (Right ()) result
      assertEqual "Final stack should be [5]" [5] (HW.Eval.getStack finalState)

  , testCase "Add should fail due to stack underflow" $ do
      let (finalState, result) = runMyState (HW.Eval.execInstr HW.StackMachine.Add) (HW.Eval.initialState :: HW.Eval.MachineState String)
      assertEqual "Add should fail due to stack underflow" (Left (HW.Eval.StackUnderflow HW.StackMachine.Add)) result

  , testCase "PushVar undefined" $ do
      let (finalState, result) = runMyState (HW.Eval.execInstr (HW.StackMachine.PushVar "x")) (HW.Eval.initialState :: HW.Eval.MachineState String)
      assertEqual "PushVar should fail due to undefined variable" (Left (HW.Eval.VarUndefined "x")) result

  , testCase "StoreVar with empty stack" $ do
      let (finalState, result) = runMyState (HW.Eval.execInstr (HW.StackMachine.StoreVar "x")) (HW.Eval.initialState :: HW.Eval.MachineState String)
      assertEqual "StoreVar should fail due to stack underflow" (Left (HW.Eval.StackUnderflow (HW.StackMachine.StoreVar "x"))) result

  , testCase "Add with insufficient values on the stack" $ do
      let (finalState, result) = runMyState (HW.Eval.execInstr HW.StackMachine.Add) (HW.Eval.initialState :: HW.Eval.MachineState String)
      assertEqual "Add should fail due to stack underflow" (Left (HW.Eval.StackUnderflow HW.StackMachine.Add)) result
  ]

-- Casos adicionales para execInstr
testExecInstrExtra :: TestTree
testExecInstrExtra = testGroup "execInstr extra tests"
  [ testCase "PushNum -5" $ do
      let (finalState, result) = runMyState (HW.Eval.execInstr (HW.StackMachine.PushNum (-5))) (HW.Eval.initialState :: HW.Eval.MachineState String)
      assertEqual "Stack should contain -5" (Right ()) result
      assertEqual "Final stack should be [-5]" [-5] (HW.Eval.getStack finalState)

  , testCase "Add large numbers" $ do
      let (finalState, result) = runMyState (HW.Eval.execInstr HW.StackMachine.Add) 
                               (HW.Eval.MachineState [1000000, 999999] M.empty :: HW.Eval.MachineState String)
      assertEqual "Stack should contain 1999999" (Right ()) result
      assertEqual "Final stack should be [1999999]" [1999999] (HW.Eval.getStack finalState)

  , testCase "StoreVar after multiple operations" $ do
      let state = HW.Eval.MachineState [5, 10] (M.empty :: M.Map String Int)
      let (finalState, result) = runMyState (HW.Eval.execInstr (HW.StackMachine.StoreVar "y")) state
      assertEqual "StoreVar should store 5 in y" (Right ()) result
      assertEqual "Final env should be [\"y\" -> 5]" (M.fromList [("y", 5)]) (HW.Eval.getEnv finalState)

  , testCase "PushVar with a defined variable" $ do
      let state = HW.Eval.MachineState [] (M.fromList [("x", 20)] :: M.Map String Int)
      let (finalState, result) = runMyState (HW.Eval.execInstr (HW.StackMachine.PushVar "x")) state
      assertEqual "PushVar should push 20 to stack" (Right ()) result
      assertEqual "Final stack should be [20]" [20] (HW.Eval.getStack finalState)
  ]

-- Test suite for execProgram function
testExecProgram :: TestTree
testExecProgram = testGroup "execProgram tests"
  [ testCase "run exampleProgram" $ do
      let program = [HW.StackMachine.PushNum 5, HW.StackMachine.PushNum 10, HW.StackMachine.Add, HW.StackMachine.StoreVar "x", HW.StackMachine.PushVar "x", HW.StackMachine.PushNum 2, HW.StackMachine.Add]
      let expectedState = HW.Eval.MachineState [17] (M.fromList [("x", 15)] :: M.Map String Int)
      HW.Eval.execProgram program (HW.Eval.initialState :: HW.Eval.MachineState String) @?= Right expectedState

  , testCase "run underflowExpr" $ do
      let program = [HW.StackMachine.PushNum 13, HW.StackMachine.Add]
      HW.Eval.execProgram program (HW.Eval.initialState :: HW.Eval.MachineState String) @?= Left (HW.Eval.StackUnderflow HW.StackMachine.Add)

  , testCase "run undefVar program" $ do
      let program = [HW.StackMachine.PushVar "x"]
      HW.Eval.execProgram program (HW.Eval.initialState :: HW.Eval.MachineState String) @?= Left (HW.Eval.VarUndefined "x")

  , testCase "complex nested Expr.let program" $ do
      let expr = Expr.Let "x" (Expr.Num 13)
                    (Expr.Let "y" (Expr.Num 42)
                      (Expr.Let "x" (Expr.Plus (Expr.Var "x") (Expr.Var "y"))
                        (Expr.Plus (Expr.Var "x") (Expr.Var "y"))))
      let program = HW.Compiler.compile expr
      let expectedState = HW.Eval.MachineState [97] (M.fromList [("x", 55), ("y", 42)] :: M.Map String Int)
      HW.Eval.execProgram program (HW.Eval.initialState :: HW.Eval.MachineState String) @?= Right expectedState
  ]

-- Casos adicionales para execProgram
testExecProgramExtra :: TestTree
testExecProgramExtra = testGroup "execProgram extra tests"
  [ testCase "Program mixing PushNum, Add, StoreVar" $ do
      let program = [HW.StackMachine.PushNum 3, HW.StackMachine.PushNum 4, HW.StackMachine.Add, HW.StackMachine.StoreVar "z", HW.StackMachine.PushVar "z"]
      let expectedState = HW.Eval.MachineState [7] (M.fromList [("z", 7)] :: M.Map String Int)
      HW.Eval.execProgram program (HW.Eval.initialState :: HW.Eval.MachineState String) @?= Right expectedState

  , testCase "Program causing StackUnderflow with Add" $ do
      let program = [HW.StackMachine.Add]
      HW.Eval.execProgram program (HW.Eval.initialState :: HW.Eval.MachineState String) @?= Left (HW.Eval.StackUnderflow HW.StackMachine.Add)

  , testCase "Program with nested let expressions" $ do
      let expr = Expr.Let "a" (Expr.Num 5)
                    (Expr.Let "b" (Expr.Num 10)
                      (Expr.Plus (Expr.Var "a") (Expr.Var "b")))
      let program = HW.Compiler.compile expr
      let expectedState = HW.Eval.MachineState [15] (M.fromList [("a", 5), ("b", 10)] :: M.Map String Int)
      HW.Eval.execProgram program (HW.Eval.initialState :: HW.Eval.MachineState String) @?= Right expectedState

  , testCase "Program storing and using multiple variables" $ do
      let expr = Expr.Let "x" (Expr.Num 7)
                    (Expr.Let "y" (Expr.Plus (Expr.Var "x") (Expr.Num 3))
                      (Expr.Plus (Expr.Var "x") (Expr.Var "y")))
      let program = HW.Compiler.compile expr
      let expectedState = HW.Eval.MachineState [17] (M.fromList [("x", 7), ("y", 10)] :: M.Map String Int)
      HW.Eval.execProgram program (HW.Eval.initialState :: HW.Eval.MachineState String) @?= Right expectedState
  ]

main :: IO ()
main = defaultMain $ testGroup "Stack Machine Tests"
  [ testCompile
  , testExecInstr
  , testExecInstrExtra
  , testExecProgram
  , testExecProgramExtra
  ]
