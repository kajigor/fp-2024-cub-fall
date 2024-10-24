import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as M
import State.MyState
import HW.Compiler
import HW.Eval
import HW.StackMachine 

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [compilerTests, evalInstrTests, evalProgramTests]

-- Helper function to run MyState computation and return final result
runStateTest :: MyState s a -> s -> a
runStateTest m initialState = evalState m initialState

-- Initial machine state
initialState :: MachineState String
initialState = MachineState [] M.empty

-- Test 1: Testing Compiler
compilerTests :: TestTree
compilerTests = testGroup "Compiler Tests"
  [ testCase "Simple Expression Compilation" $
      let expr = Add (Num 5) (Num 10)
      in compile expr @?= [PushNum 5, PushNum 10, Add]
  
  , testCase "Let Expression Compilation" $
      let expr = Let "x" (Num 5) (Add (Var "x") (Num 2))
      in compile expr @?= [PushNum 5, StoreVar "x", PushVar "x", PushNum 2, Add]
  ]

-- Test 2: Testing execInstr (execution of a single instruction)
evalInstrTests :: TestTree
evalInstrTests = testGroup "execInstr Tests"
  [ testCase "PushNum Test" $
      let instr = PushNum 10
          finalState = runStateTest (execInstr instr) initialState
      in finalState @?= Right ()

  , testCase "PushVar Undefined Test" $
      let instr = PushVar "x"
          finalState = runStateTest (execInstr instr) initialState
      in finalState @?= Left (VarUndefined "x")

  , testCase "Add Test" $
      let instrs = [PushNum 3, PushNum 7, Add]
          stateWithStack = initialState { getStack = [3, 7] }
          finalState = runStateTest (execInstr Add) stateWithStack
      in finalState @?= Right ()

  , testCase "StoreVar Test" $
      let instrs = [PushNum 8, StoreVar "y"]
          stateWithStack = initialState { getStack = [8] }
          finalState = runStateTest (execInstr (StoreVar "y")) stateWithStack
      in finalState @?= Right ()
  ]

-- Test 3: Testing execProgram (execution of the full program)
evalProgramTests :: TestTree
evalProgramTests = testGroup "execProgram Tests"
  [ testCase "Simple Program Execution" $
      let program = [PushNum 5, PushNum 3, Add]
          finalState = execProgram program initialState
      in case finalState of
           Right (MachineState stack _) -> stack @?= [8]
           Left err -> assertFailure $ "Unexpected error: " ++ show err

  , testCase "Variable Store and Use" $
      let program = [PushNum 5, StoreVar "x", PushVar "x", PushNum 2, Add]
          finalState = execProgram program initialState
      in case finalState of
           Right (MachineState stack _) -> stack @?= [7]
           Left err -> assertFailure $ "Unexpected error: " ++ show err

  , testCase "Underflow Error" $
      let program = [PushNum 5, Add]
          finalState = execProgram program initialState
      in finalState @?= Left (StackUnderflow Add)

  , testCase "Undefined Variable Error" $
      let program = [PushVar "y"]
          finalState = execProgram program initialState
      in finalState @?= Left (VarUndefined "y")
  ]

