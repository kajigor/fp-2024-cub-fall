import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Expr
import qualified Error
import qualified Interpretor

-- Numeric cases: Simple evaluations for numbers and square roots
numCases :: [(Expr.Expr, Either Error.Error Double)]
numCases = 
  [ (Expr.Num 7, Right 7)
  , (Expr.Num 0, Right 0)
  , (Expr.Num (-1), Right (-1))
  , (Expr.Sqrt (Expr.Num 16), Right 4)
  , (Expr.Sqrt (Expr.Num 0), Right 0)
  , (Expr.Sqrt (Expr.Num (-3)), Left (Error.NegativeSqrt (Expr.Num (-3))))
  ]

-- Arithmetic cases: Testing various arithmetic operations
arithmeticCases :: [(Expr.Expr, Either Error.Error Double)]
arithmeticCases = 
  [ (Expr.Add (Expr.Num 3) (Expr.Num 5), Right 8)
  , (Expr.Sub (Expr.Num 11) (Expr.Num 4), Right 7)
  , (Expr.Mul (Expr.Num 3) (Expr.Num 5), Right 15)
  , (Expr.Div (Expr.Num 16) (Expr.Num 2), Right 8)
  , (Expr.Div (Expr.Num 11) (Expr.Num 0), Left (Error.DivByZero (Expr.Num 11) (Expr.Num 0)))
  , (Expr.Pow (Expr.Num 4) (Expr.Num 3), Right 64)
  , (Expr.Add (Expr.Mul (Expr.Num 3) (Expr.Num 3)) (Expr.Num 3), Right 12)
  , (Expr.Div (Expr.Num 10) (Expr.Sub (Expr.Num 1) (Expr.Num 1)), Left (Error.DivByZero (Expr.Num 10) (Expr.Sub (Expr.Num 1) (Expr.Num 1))))
  , (Expr.Add (Expr.Num (-5)) (Expr.Num 6), Right 1) 
  , (Expr.Sub (Expr.Num 5) (Expr.Num 11), Right (-6))
  ]

-- Let cases: Testing variable bindings and nested let expressions
letCases :: [(Expr.Expr, Either Error.Error Double)]
letCases = 
  [ (Expr.Let "x" (Expr.Num 5) (Expr.Var "x"), Right 5)
  , (Expr.Let "y" (Expr.Num 10) (Expr.Add (Expr.Var "y") (Expr.Num 6)), Right 16)
  , (Expr.Let "z" (Expr.Num 0) (Expr.Sqrt (Expr.Var "z")), Right 0)
  , (Expr.Let "a" (Expr.Num 5) (Expr.Div (Expr.Num 10) (Expr.Var "a")), Right 2)
  , (Expr.Let "b" (Expr.Num 4) (Expr.Sub (Expr.Var "b") (Expr.Var "b")), Right 0)
  , (Expr.Let "x" (Expr.Num 6) (Expr.Let "x" (Expr.Num 2) (Expr.Var "x")), Right 2)
  , (Expr.Let "x" (Expr.Num 5) (Expr.Add (Expr.Var "x") (Expr.Num 2)), Right 7)
  , (Expr.Let "x" (Expr.Num 12) (Expr.Let "y" (Expr.Num 22) (Expr.Add (Expr.Var "x") (Expr.Var "y"))), Right 34)
  , (Expr.Let "x" (Expr.Num 4) (Expr.Sqrt (Expr.Var "x")), Right 2)
  , (Expr.Let "y" (Expr.Num (-5)) (Expr.Sqrt (Expr.Var "y")), Left (Error.NegativeSqrt (Expr.Var "y")))
  , (Expr.Let "a" (Expr.Num 0) (Expr.Div (Expr.Num 10) (Expr.Var "a")), Left (Error.DivByZero (Expr.Num 10) (Expr.Var "a")))
  ]

testExpr :: (Expr.Expr, Either Error.Error Double) -> TestTree
testExpr (expr, expected) = testCase (printf "Testing: %s" (show expr)) $ 
    let actual = Interpretor.eval Map.empty expr in
    case (expected, actual) of
        (Right expVal, Right actVal) -> assertEqual "" expVal actVal
        (Left expErr, Left actErr)   -> assertEqual "" expErr actErr
        _ -> assertFailure $ printf "Expected %s but got %s" (show expected) (show actual)

numTests :: TestTree
numTests = testGroup "Numerical Expressions Tests" $ map testExpr numCases

arithmeticTests :: TestTree
arithmeticTests = testGroup "Arithmetic Operations Tests" $ map testExpr arithmeticCases

letTests :: TestTree
letTests = testGroup "Let Expressions Tests" $ map testExpr letCases

main :: IO ()
main = defaultMain $ testGroup "Expression Tests" [numTests, arithmeticTests, letTests]
