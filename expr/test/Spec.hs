import qualified Data.Map.Strict as M
import Err (Error (..))
import Expr (Expr (..))
import Interpreter (eval)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testEval :: TestTree
testEval =
  testGroup
    "Eval"
    [ testAdd,
      testSub,
      testMul,
      testDiv,
      testPow,
      testSqrt,
      testVar,
      testLet,
      testLetNested
    ]
  where
    testSuccess msg expr res =
      testCase msg $ eval M.empty expr @?= Right res

    testFailure msg expr expectedErr =
      testCase msg $ eval M.empty expr @?= Left expectedErr

    testAdd =
      testGroup
        "Addition"
        [ testSuccess "5 + 3 == 8" (BinOpAdd (Numb 5) (Numb 3)) 8,
          testSuccess "1 + 2 == 3" (BinOpAdd (Numb 1) (Numb 2)) 3
        ]

    testSub =
      testGroup
        "Subtraction"
        [ testSuccess "5 - 3 == 2" (BinOpSub (Numb 5) (Numb 3)) 2,
          testSuccess "10 - 7 == 3" (BinOpSub (Numb 10) (Numb 7)) 3
        ]

    testMul =
      testGroup
        "Multiplication"
        [ testSuccess "2 * 4 == 8" (BinOpMul (Numb 2) (Numb 4)) 8,
          testSuccess "6 * 7 == 42" (BinOpMul (Numb 6) (Numb 7)) 42
        ]

    testDiv =
      testGroup
        "Division"
        [ testSuccess "10 / 2 == 5" (BinOpDiv (Numb 10) (Numb 2)) 5,
          testFailure "Division by zero" (BinOpDiv (Numb 10) (Numb 0)) (DivByZero (BinOpDiv (Numb 10) (Numb 0)))
        ]

    testPow =
      testGroup
        "Power"
        [ testSuccess "2 ^ 3 == 8" (BinOpPow (Numb 2) (Numb 3)) 8,
          testSuccess "5 ^ 2 == 25" (BinOpPow (Numb 5) (Numb 2)) 25
        ]

    testSqrt =
      testGroup
        "Square Root"
        [ testSuccess "sqrt(4) == 2" (SqrtExpr (Numb 4)) 2,
          testFailure "Sqrt of negative number" (SqrtExpr (Numb (-9))) (SqrtNeg (SqrtExpr (Numb (-9))))
        ]

    testVar =
      testGroup
        "Variable Lookup"
        [ testCase "x == 42, where x is defined" $ eval (M.singleton "x" 42) (Var "x") @?= Right 42,
          testCase "y == 13, where y is defined" $ eval (M.singleton "y" 13) (Var "y") @?= Right 13,
          testFailure "Undefined variable x" (Var "x") (VarNotFound (Var "x"))
        ]

    testLet =
      testGroup
        "Let Expressions"
        [ testSuccess "let x = 5 in x + 3 == 8" (LetExpr "x" (Numb 5) (BinOpAdd (Var "x") (Numb 3))) 8,
          testSuccess "let y = 10 in y * 2 == 20" (LetExpr "y" (Numb 10) (BinOpMul (Var "y") (Numb 2))) 20,
          testFailure "let z = 0 in 10 / z (Division by zero)" (LetExpr "z" (Numb 0) (BinOpDiv (Numb 10) (Var "z"))) (DivByZero (BinOpDiv (Numb 10) (Var "z")))
        ]
    testLetNested =
      testGroup
        "Nested Let Expressions"
        [ testSuccess
            "let x = 5 in let y = x + 2 in y * x == 35"
            (LetExpr "x" (Numb 5) (LetExpr "y" (BinOpAdd (Var "x") (Numb 2)) (BinOpMul (Var "y") (Var "x"))))
            35,
          testSuccess
            "let x = 4 in let y = x + 3 in let z = y * 2 in z - x == 10"
            (LetExpr "x" (Numb 4) (LetExpr "y" (BinOpAdd (Var "x") (Numb 3)) (LetExpr "z" (BinOpMul (Var "y") (Numb 2)) (BinOpSub (Var "z") (Var "x")))))
            10,
          testSuccess
            "let a = 3 in let b = a^2 in let c = b + 7 in sqrt(c) == 4"
            (LetExpr "a" (Numb 3) (LetExpr "b" (BinOpPow (Var "a") (Numb 2)) (LetExpr "c" (BinOpAdd (Var "b") (Numb 7)) (SqrtExpr (Var "c")))))
            4,
          testFailure
            "let x = 10 in let y = 0 in x / y (Division by zero)"
            (LetExpr "x" (Numb 10) (LetExpr "y" (Numb 0) (BinOpDiv (Var "x") (Var "y"))))
            (DivByZero (BinOpDiv (Var "x") (Var "y")))
        ]

main :: IO ()
main =
  defaultMain $ testGroup "Expression Tests" [testEval]