import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Expr
import qualified Error
import qualified Interpreter

testCases :: [(Expr.Expr, Either Error.Error Double)]
testCases = 
    [ -- Simple numbers and operations
      (Expr.Num 5, Right 5.0)
    , (Expr.Num (-3), Right (-3.0))
    , (Expr.Add (Expr.Num 2) (Expr.Num 3), Right 5.0)
    , (Expr.Sub (Expr.Num 10) (Expr.Num 7), Right 3.0)
    , (Expr.Mul (Expr.Num 4) (Expr.Num 5), Right 20.0)
    , (Expr.Div (Expr.Num 20) (Expr.Num 4), Right 5.0)
    , (Expr.Pow (Expr.Num 2) (Expr.Num 3), Right 8.0)

      -- Square root tests
    , (Expr.Sqrt (Expr.Num 9), Right 3.0)
    , (Expr.Sqrt (Expr.Num 0), Right 0.0)
    , (Expr.Sqrt (Expr.Num (-4)), Left (Error.NegativeSqrt (Expr.Num (-4))))
    
      -- Division by zero
    , (Expr.Div (Expr.Num 10) (Expr.Num 0), Left (Error.DivByZero (Expr.Num 10) (Expr.Num 0)))

      -- Nested expressions
    , (Expr.Add (Expr.Mul (Expr.Num 2) (Expr.Num 3)) (Expr.Div (Expr.Num 10) (Expr.Num 2)), Right 11.0)
    , (Expr.Sub (Expr.Pow (Expr.Num 2) (Expr.Num 3)) (Expr.Sqrt (Expr.Num 16)), Right 4.0)

      -- Chained operations
    , (Expr.Add (Expr.Num 1) (Expr.Mul (Expr.Num 2) (Expr.Sub (Expr.Num 7) (Expr.Num 4))), Right 7.0)
    , (Expr.Div (Expr.Pow (Expr.Num 16) (Expr.Num 0.5)) (Expr.Num 4), Right 1.0)

      -- Zero as operand
    , (Expr.Add (Expr.Num 0) (Expr.Num 10), Right 10.0)
    , (Expr.Mul (Expr.Num 0) (Expr.Num 100), Right 0.0)
    
      -- Variable lookup
    , (Expr.Variable "x", Left (Error.UnassignedVariable "x"))
    , (Expr.Let "x" (Expr.Num 5) (Expr.Add (Expr.Variable "x") (Expr.Num 3)), Right 8.0)
    
      -- Nested Let bindings
    , (Expr.Let "x" (Expr.Num 5) (Expr.Let "y" (Expr.Num 3) (Expr.Mul (Expr.Variable "x") (Expr.Variable "y"))), Right 15.0)
    , (Expr.Let "x" (Expr.Num 2) (Expr.Let "y" (Expr.Add (Expr.Variable "x") (Expr.Num 3)) (Expr.Pow (Expr.Variable "y") (Expr.Variable "x"))), Right 25.0)
    , (Expr.Let "x" (Expr.Num 13) (Expr.Let "y" (Expr.Add (Expr.Variable "x") (Expr.Num 1)) (Expr.Pow (Expr.Variable "y") (Expr.Num 2))), Right 196.0)

      -- Unassigned Variable within let expression
    , (Expr.Let "x" (Expr.Num 2) (Expr.Add (Expr.Variable "x") (Expr.Variable "y")), Left (Error.UnassignedVariable "y"))
    
      -- Error handling in nested expressions
    , (Expr.Let "x" (Expr.Div (Expr.Num 10) (Expr.Num 0)) (Expr.Add (Expr.Variable "x") (Expr.Num 5)), Left (Error.DivByZero (Expr.Num 10) (Expr.Num 0)))
    ]

testEval :: TestTree
testEval = testGroup "Evaluation Tests"
    [ testCase (show expr) $
        assertEqual ("for: " ++ show expr) expected (Interpreter.eval Map.empty expr)
    | (expr, expected) <- testCases
    ]

main :: IO ()
main = defaultMain testEval
