import Test.Tasty
import Test.Tasty.HUnit
import HW.Compiler
import HW.Eval
import HW.StackMachine
import State.MyState
import qualified Data.Map as M
import Text.Printf (printf)
import Control.Monad (unless)
import Expr

run :: StackInstr String -> MachineState String -> Either (Error String) (MachineState String)
run instruction state = 
    let (newState, result) = runMyState (execInstr instruction) state
    in case result of
         Left err -> Left err
         Right _  -> Right newState

tests :: TestTree
tests = testGroup "Test Cases"
    [ testCase "Add number value to stack" $
        let initial = MachineState [] M.empty
            expected = MachineState [33] M.empty
        in run (PushNum 33) initial @?= Right expected

    , testCase "Add the value of a variable to the stack" $
        let initial = MachineState [] (M.fromList [("x", 10)])
            expected = MachineState [10] (M.fromList [("x", 10)])
        in run (PushVar "x") initial @?= Right expected

    , testCase "Addition numbers" $
        let initial = MachineState [10, 11] M.empty
            expected = MachineState [21] M.empty
        in run Add initial @?= Right expected

    , testCase "Addition variables values" $
        let initial = MachineState [] (M.fromList [("x", 10), ("y", 12)])
            expected = MachineState [22] (M.fromList [("x", 10), ("y", 12)])
            prog = [PushVar "x", PushVar "y", Add]
            result = execProgram prog initial
         in result @?= Right expected 
    
    , testCase "Store the top value in a variable" $
        let initial = MachineState [3] M.empty
            expected = MachineState [] (M.fromList[("x", 3)])
        in run (StoreVar "x") initial @?= Right expected

    , testCase "Add two numbers and store them in a variable" $
        let initial = MachineState [3, 20, 3] M.empty
            expected = MachineState [3] (M.fromList[("x", 23)])
            prog = [Add, StoreVar "x"]
            result = execProgram prog initial
        in result @?= Right expected

    , testCase "StackUndeflow on empty Add" $
        let initial = MachineState [] M.empty
        in run Add initial @?= Left (StackUnderflow Add)

    , testCase "StackUndeflow on Add" $
        let initial = MachineState [1] M.empty
        in run Add initial @?= Left (StackUnderflow Add)

    , testCase "VarUndefined for undefined variable" $
        let initial = MachineState [] M.empty
        in run (PushVar "x") initial @?= Left (VarUndefined "\"x\"")

    , testCase "StackNotExhausted error" $
        let initial = MachineState [3, 11] (M.fromList[("x", 100)])
            prog = [PushVar "x", Add]
            result = execProgram prog initial
        in result @?= Left (StackNotExhausted [103, 11])

    , testCase "Addition with Compile" $
        let expr = Plus (Num 3) (Num 1)
            prog = compile expr
            initial :: MachineState String
            initial = MachineState [] M.empty
            expected :: MachineState String
            expected = MachineState [4] M.empty
            result :: Either (Error String) (MachineState String)
            result = execProgram prog initial
        in result @?= Right expected

    , testCase "Let Compile" $
        let expr = Let "x" (Num 3) (Plus (Var "x") (Num 1)) 
            prog = compile expr
            initial :: MachineState String
            initial = MachineState [] M.empty
            expected :: MachineState String
            expected = MachineState [4] (M.fromList[("x", 3)])
            result :: Either (Error String) (MachineState String)
            result = execProgram prog initial
        in result @?= Right expected

    , testCase "Compile push var value and use addition" $
        let expr = Plus (Var "x") (Num 1)
            prog = compile expr
            initial :: MachineState String
            initial = MachineState [] (M.fromList[("x", 10)])
            expected :: MachineState String
            expected = MachineState [11] (M.fromList[("x", 10)])
            result :: Either (Error String) (MachineState String)
            result = execProgram prog initial
        in result @?= Right expected

    , testCase "Compile nested let bindings" $
        let expr = Let "x" (Num 3) (Let "y" (Num 1) (Plus (Var "x") (Var "y")))
            prog = compile expr
            initial :: MachineState String
            initial = MachineState [] M.empty
            expected :: MachineState String
            expected = MachineState [4] (M.fromList[("x", 3), ("y", 1)])
            result :: Either (Error String) (MachineState String)
            result = execProgram prog initial
        in result @?= Right expected
    ]

main :: IO ()
main =
  defaultMain $ testGroup "Expression Language" [tests]
