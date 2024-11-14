import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map.Strict as M
import qualified HW.Eval
import qualified Expr
import qualified HW.Compiler
import qualified HW.StackMachine
import State.MyState (runMyState)

type Stack = HW.Eval.Stack

-- Test Intruction
testInstr :: (Ord v, Show v) 
          => String -- Name of the Test Case
          -> HW.StackMachine.StackInstr v -- Instruction
          -> HW.Eval.MachineState v -- Initial State
          -> Either (HW.Eval.Error v) () -- Expected result
          -> Stack -- Expected Stack
          -> TestTree
testInstr name instr initialState expectedResult expectedStack = testCase name $ do
  let (finalState, result) = runMyState (HW.Eval.execInstr instr) initialState
  assertEqual "Unexpected result" expectedResult result
  assertEqual "Unexpected final stack" expectedStack (HW.Eval.getStack finalState)

-- Test program
testProgram :: (Ord v, Show v) 
            => String -- Name of the test Case
            -> HW.StackMachine.StackProgram v -- Program
            -> HW.Eval.MachineState v -- Initial State
            -> Either (HW.Eval.Error v) (HW.Eval.MachineState v) -- Final State
            -> TestTree
testProgram name program initialState expected = testCase name $
  HW.Eval.execProgram program initialState @?= expected

testComp :: (Ord v, Show v) => String -> Expr.Expr v -> HW.StackMachine.StackProgram v -> TestTree
testComp name expr expected = testCase name $ HW.Compiler.compile expr @?= expected

testCompile :: TestTree
testCompile = testGroup "compile tests"
  [ testComp "compile simple let expression" 
        (Expr.Let "x" (Expr.Num 5) (Expr.Plus (Expr.Var "x") (Expr.Num 10)))
        [ HW.StackMachine.PushNum 5, HW.StackMachine.StoreVar "x", HW.StackMachine.PushVar "x", HW.StackMachine.PushNum 10, HW.StackMachine.Add]
  , testComp "compile nested let expression"
        (Expr.Let "x" (Expr.Num 13)
            (Expr.Let "y" (Expr.Num 42)
                (Expr.Let "x" (Expr.Plus (Expr.Var "x") (Expr.Var "y"))
                    (Expr.Plus (Expr.Var "x") (Expr.Var "y")))))
        [HW.StackMachine.PushNum 13, HW.StackMachine.StoreVar "x", 
        HW.StackMachine.PushNum 42, HW.StackMachine.StoreVar "y", 
        HW.StackMachine.PushVar "x", HW.StackMachine.PushVar "y", HW.StackMachine.Add, HW.StackMachine.StoreVar "x", 
        HW.StackMachine.PushVar "x", HW.StackMachine.PushVar "y", HW.StackMachine.Add]
  ]

testExecInstr :: TestTree
testExecInstr = testGroup "execInstr tests"
  [ testInstr "PushNum 5" (HW.StackMachine.PushNum 5) (HW.Eval.initialState :: HW.Eval.MachineState String) (Right ()) [5]
  , testInstr "Add should fail due to stack underflow" HW.StackMachine.Add (HW.Eval.initialState :: HW.Eval.MachineState String) (Left (HW.Eval.StackUnderflow HW.StackMachine.Add)) []
  , testInstr "PushVar undefined" (HW.StackMachine.PushVar "x") (HW.Eval.initialState :: HW.Eval.MachineState String) (Left (HW.Eval.VarUndefined "x")) []
  , testInstr "StoreVar with empty stack" (HW.StackMachine.StoreVar "x") (HW.Eval.initialState :: HW.Eval.MachineState String) (Left (HW.Eval.StackUnderflow (HW.StackMachine.StoreVar "x"))) []
  , testInstr "PushNum -5" (HW.StackMachine.PushNum (-5)) (HW.Eval.initialState :: HW.Eval.MachineState String) (Right ()) [-5]
  , testInstr "Add large numbers" HW.StackMachine.Add (HW.Eval.MachineState [1000000, 999999] (M.empty :: M.Map String Int)) (Right ()) [1999999]
  , testInstr "StoreVar after multiple operations" (HW.StackMachine.StoreVar "y") (HW.Eval.MachineState [5, 10] (M.empty :: M.Map String Int)) (Right ()) [10]
  , testInstr "PushVar with a defined variable" (HW.StackMachine.PushVar "x") (HW.Eval.MachineState [] (M.fromList [("x", 20)] :: M.Map String Int)) (Right ()) [20]
  ]

testExecProgram :: TestTree
testExecProgram = testGroup "execProgram tests"
  [ testProgram "run exampleProgram"
      [HW.StackMachine.PushNum 5, HW.StackMachine.PushNum 10, HW.StackMachine.Add, HW.StackMachine.StoreVar "x", HW.StackMachine.PushVar "x", HW.StackMachine.PushNum 2, HW.StackMachine.Add]
      (HW.Eval.initialState :: HW.Eval.MachineState String)
      (Right (HW.Eval.MachineState [17] (M.fromList [("x", 15)])))

  , testProgram "run underflowExpr"
      [HW.StackMachine.PushNum 13, HW.StackMachine.Add]
      (HW.Eval.initialState :: HW.Eval.MachineState String)
      (Left (HW.Eval.StackUnderflow HW.StackMachine.Add))

  , testProgram "run undefVar program"
      [HW.StackMachine.PushVar "x"]
      (HW.Eval.initialState :: HW.Eval.MachineState String)
      (Left (HW.Eval.VarUndefined "x"))

  , testProgram "complex nested Expr.let program"
      (HW.Compiler.compile (Expr.Let "x" (Expr.Num 13) (Expr.Let "y" (Expr.Num 42) (Expr.Let "x" (Expr.Plus (Expr.Var "x") (Expr.Var "y")) (Expr.Plus (Expr.Var "x") (Expr.Var "y"))))))
      (HW.Eval.initialState :: HW.Eval.MachineState String)
      (Right (HW.Eval.MachineState [97] (M.fromList [("x", 55), ("y", 42)])))

  , testProgram "Program mixing PushNum, Add, StoreVar"
      [HW.StackMachine.PushNum 3, HW.StackMachine.PushNum 4, HW.StackMachine.Add, HW.StackMachine.StoreVar "z", HW.StackMachine.PushVar "z"]
      (HW.Eval.initialState :: HW.Eval.MachineState String)
      (Right (HW.Eval.MachineState [7] (M.fromList [("z", 7)])))

  , testProgram "Program causing StackUnderflow with Add"
      [HW.StackMachine.Add]
      (HW.Eval.initialState :: HW.Eval.MachineState String)
      (Left (HW.Eval.StackUnderflow HW.StackMachine.Add))

  , testProgram "Program with nested let expressions"
      (HW.Compiler.compile (Expr.Let "a" (Expr.Num 5) (Expr.Let "b" (Expr.Num 10) (Expr.Plus (Expr.Var "a") (Expr.Var "b")))))
      (HW.Eval.initialState :: HW.Eval.MachineState String)
      (Right (HW.Eval.MachineState [15] (M.fromList [("a", 5), ("b", 10)])))

  , testProgram "Program storing and using multiple variables"
      (HW.Compiler.compile (Expr.Let "x" (Expr.Num 7) (Expr.Let "y" (Expr.Plus (Expr.Var "x") (Expr.Num 3)) (Expr.Plus (Expr.Var "x") (Expr.Var "y")))))
      (HW.Eval.initialState :: HW.Eval.MachineState String)
      (Right (HW.Eval.MachineState [17] (M.fromList [("x", 7), ("y", 10)])))
  ]

main :: IO ()

main = defaultMain $ testGroup "Stack Machine Tests"
  [ testCompile
  , testExecInstr
  , testExecProgram
  ]