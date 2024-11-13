import qualified Data.Map         as M
import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (assertBool, assertFailure, testCase)

import           Expr
import           HW.Compiler
import           HW.Eval
import           HW.StackMachine

testCompile :: TestTree
testCompile =
    testGroup "Compile" [testNum, testPlus, testLet, testComplex]
    where
        testNum =
            testGroup
                "Num"
                [   testSuccess "puts 0 on the stack" ([PushNum 0] :: StackProgram String) (Num 0),
                    testSuccess "puts 14 on the stack" ([PushNum 14] :: StackProgram String) (Num 14)
                ]
        testPlus =
            testGroup
                "Plus"
                [   testSuccess "2+2" ([PushNum 2, PushNum 2, Add] :: StackProgram String) (Plus (Num 2) (Num 2)),
                    testSuccess "(12+34)+56" ([PushNum 12, PushNum 34, Add, PushNum 56, Add] :: StackProgram String) (Plus (Plus (Num 12) (Num 34)) (Num 56)),
                    testSuccess "12+(34+56)" ([PushNum 12, PushNum 34, PushNum 56, Add, Add] :: StackProgram String) (Plus (Num 12) $ Plus (Num 34) (Num 56))
                ]
        testLet =
            testGroup
                "Let"
                [   testSuccess "x=13, y=42, x=x+y, x+y"
                        ([PushNum 13, StoreVar "x", PushNum 42, StoreVar "y", PushVar "x", PushVar "y", Add, StoreVar "x", PushVar "x", PushVar "y", Add] :: StackProgram String) expr,
                    testSuccess "x=12, y=42, x=x+x, x+x"
                        ([PushNum 13, StoreVar "x", PushNum 42, StoreVar "x", PushVar "x", PushVar "x", Add, StoreVar "x", PushVar "x", PushVar "x", Add] :: StackProgram String) expr1
                ]
        testComplex =
            testGroup
                "Complex"
                [   testSuccess "x=13, y=42, z=x+y, t=y+z, x+(y+(z+t))" ([   PushNum 13, StoreVar "x", PushNum 42, StoreVar "y", PushVar "x", PushVar "y", Add,
                                                                            StoreVar "z", PushVar "y", PushVar "z", Add, StoreVar "t", PushVar "x", PushVar "y",
                                                                            PushVar "z", PushVar "t", Add, Add, Add
                                                                         ] :: StackProgram String
                                                                        )
                                                                            ( Let "x" (Num 13) $
                                                                              Let "y" (Num 42) $
                                                                              Let "z" (Plus (Var "x") (Var "y")) $
                                                                              Let "t" (Plus (Var "y") (Var "z")) $
                                                                              Plus (Var "x") $ Plus (Var "y") $ Plus (Var "z") (Var "t")
                                                                            )
                ]

        testSuccess :: (Ord v, Show v) => String -> StackProgram v -> Expr v -> TestTree
        testSuccess msg expected testExpr = testCase msg (assertBool ("Evaluation result is wrong. Expected " ++ show expected ++ ", but got " ++ show result) (result == expected)) where
            result = compile testExpr

testRun :: TestTree
testRun = testGroup "Run" [testSimple, testError, testMisc]
    where
        testSimple =
            testGroup
                "Simple"
                [   testSuccess "puts 0 on the stack" initialState (MachineState [0] M.empty) ([PushNum 0] :: StackProgram String),
                    testSuccess "2+2" initialState (MachineState [4] M.empty) ([PushNum 2, PushNum 2, Add] :: StackProgram String)
                ]
        testError =
            testGroup
                "Error"
                [   testFailure "StackUnderflow on Add" initialState (StackUnderflow Add) ([PushNum 5, Add] :: StackProgram String),
                    testFailure "StackUnderflow on StoreVar" initialState (StackUnderflow $ StoreVar "x") ([StoreVar "x"] :: StackProgram String),
                    testFailure "VarUndefined" initialState (VarUndefined "\"y\"") ([PushNum 13, StoreVar "x", PushVar "y"] :: StackProgram String),
                    testFailure "StackNotExhausted" initialState (StackNotExhausted [21, 21]) ([PushNum 21, PushNum 8, PushNum 13, Add] :: StackProgram String),
                    testFailure "StackEmpty" initialState FinalStackEmpty ([] :: StackProgram String)
                ]
        testMisc =
            testGroup
                "Misc"
                [   testSuccess "Non-empty start state" (MachineState [23, 34] (M.fromList [("x", 42)])) (MachineState [0] $ M.fromList [("x", 99)])
                        ([PushVar "x", Add, Add, StoreVar "x", PushNum 0] :: StackProgram String),
                    testSuccess "Weird var name" (MachineState [] (M.empty :: M.Map () Int)) (MachineState [42] (M.fromList [((), 42)])) ([PushNum 42, StoreVar (), PushNum 42] :: StackProgram ()),
                    testSuccess "Fibonacci" initialState (MachineState [13] (M.fromList [("x", 8), ("y", 13)]))
                        ([  PushNum 1, StoreVar "x", PushNum 1, StoreVar "y",
                            PushVar "x", PushVar "y", Add, StoreVar "y",
                            PushVar "x", PushVar "y", Add, StoreVar "x",
                            PushVar "x", PushVar "y", Add, StoreVar "y",
                            PushVar "x", PushVar "y", Add, StoreVar "x",
                            PushVar "x", PushVar "y", Add, StoreVar "y",
                            PushVar "y"
                        ] :: StackProgram String)
                ]

        testSuccess :: (Ord v, Show v) => String ->  MachineState v -> MachineState v -> StackProgram v -> TestTree
        testSuccess msg startState expected program = testCase msg $ case execProgram program startState of
            Left err -> assertFailure ("Unexpected error while evaluating the expression: " ++ show err)
            Right finishState -> assertBool ("Evaluation result is wrong. Expected " ++ show expected ++ ", but got " ++ show finishState) (finishState == expected)

        testFailure :: (Ord v, Show v) => String -> MachineState v -> Error v -> StackProgram v -> TestTree
        testFailure msg startState expected program = testCase msg $ case execProgram program startState of
            Left err -> assertBool (show expected ++ " expected, but got " ++ show err) (expected == err)
            Right finishState -> assertFailure ("Test case should've resulted in an error, but got " ++ show finishState)


main :: IO ()
main =
  defaultMain $ testGroup "Tests" [testCompile, testRun]
