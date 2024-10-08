import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Expr
import qualified Error
import qualified Interpreter

-- Numeric cases: Simple evaluations for numbers and square roots
numCases :: [(Expr.Expr, Either Error.Error Double)]
numCases = 
  [ (Expr.NumExpr 5, Right 5)
  , (Expr.NumExpr 0, Right 0)
  , (Expr.NumExpr (-10), Right (-10))
  , (Expr.SqrtExpr (Expr.NumExpr 9), Right 3)
  , (Expr.SqrtExpr (Expr.NumExpr 0), Right 0)
  , (Expr.SqrtExpr (Expr.NumExpr (-9)), Left (Error.NegativeSqrtNum (Expr.NumExpr (-9))))
  ]

-- Arithmetic cases: Testing various arithmetic operations
arithmeticCases :: [(Expr.Expr, Either Error.Error Double)]
arithmeticCases = 
  [ (Expr.AddExpr (Expr.NumExpr 2) (Expr.NumExpr 4), Right 6)
  , (Expr.SubExpr (Expr.NumExpr 10) (Expr.NumExpr 4), Right 6)
  , (Expr.MulExpr (Expr.NumExpr 2) (Expr.NumExpr 5), Right 10)
  , (Expr.DivExpr (Expr.NumExpr 10) (Expr.NumExpr 2), Right 5)
  , (Expr.DivExpr (Expr.NumExpr 10) (Expr.NumExpr 0), Left (Error.DivByZero (Expr.NumExpr 10) (Expr.NumExpr 0)))
  , (Expr.PowerExpr (Expr.NumExpr 2) (Expr.NumExpr 3), Right 8)
  , (Expr.AddExpr (Expr.MulExpr (Expr.NumExpr 2) (Expr.NumExpr 3)) (Expr.NumExpr 4), Right 10)
  , (Expr.DivExpr (Expr.NumExpr 10) (Expr.SubExpr (Expr.NumExpr 1) (Expr.NumExpr 1)), Left (Error.DivByZero (Expr.NumExpr 10) (Expr.SubExpr (Expr.NumExpr 1) (Expr.NumExpr 1))))
  , (Expr.AddExpr (Expr.NumExpr (-5)) (Expr.NumExpr 5), Right 0) 
  , (Expr.SubExpr (Expr.NumExpr 5) (Expr.NumExpr 10), Right (-5))
  ]

-- Let cases: Testing variable bindings and nested let expressions
letCases :: [(Expr.Expr, Either Error.Error Double)]
letCases = 
  [ (Expr.Let "x" (Expr.NumExpr 5) (Expr.VarExpr "x"), Right 5)
  , (Expr.Let "y" (Expr.NumExpr 10) (Expr.AddExpr (Expr.VarExpr "y") (Expr.NumExpr 5)), Right 15)
  , (Expr.Let "z" (Expr.NumExpr 0) (Expr.SqrtExpr (Expr.VarExpr "z")), Right 0)
  , (Expr.Let "a" (Expr.NumExpr 4) (Expr.DivExpr (Expr.NumExpr 10) (Expr.VarExpr "a")), Right 2.5)
  , (Expr.Let "b" (Expr.NumExpr 5) (Expr.SubExpr (Expr.VarExpr "b") (Expr.VarExpr "b")), Right 0)
  , (Expr.Let "x" (Expr.NumExpr 5) (Expr.Let "x" (Expr.NumExpr 3) (Expr.VarExpr "x")), Right 3)
  , (Expr.Let "x" (Expr.NumExpr 5) (Expr.AddExpr (Expr.VarExpr "x") (Expr.NumExpr 2)), Right 7)
  , (Expr.Let "x" (Expr.NumExpr 10) (Expr.Let "y" (Expr.NumExpr 20) (Expr.AddExpr (Expr.VarExpr "x") (Expr.VarExpr "y"))), Right 30)
  , (Expr.Let "x" (Expr.NumExpr 4) (Expr.SqrtExpr (Expr.VarExpr "x")), Right 2)
  , (Expr.Let "y" (Expr.NumExpr (-4)) (Expr.SqrtExpr (Expr.VarExpr "y")), Left (Error.NegativeSqrtNum (Expr.VarExpr "y")))
  , (Expr.Let "a" (Expr.NumExpr 0) (Expr.DivExpr (Expr.NumExpr 10) (Expr.VarExpr "a")), Left (Error.DivByZero (Expr.NumExpr 10) (Expr.VarExpr "a")))
  ]

testExpr :: (Expr.Expr, Either Error.Error Double) -> TestTree
testExpr (expr, expected) = testCase (printf "Testing: %s" (show expr)) $ 
    let actual = Interpreter.eval Map.empty expr in
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
