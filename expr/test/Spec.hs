import qualified Data.Map as Map
import qualified Error
import qualified Expr
import qualified Interpreter
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)

numCases :: [(Expr.Expr, Either Error.Error Double)]
numCases =
  [ (Expr.Num 10, Right 10),
    (Expr.Num 0, Right 0),
    (Expr.Num (-3), Right (-3)),
    (Expr.Sqrt (Expr.Num 25), Right 5),
    (Expr.Sqrt (Expr.Num 0), Right 0),
    (Expr.Sqrt (Expr.Num (-7)), Left (Error.SqrtOfNegNumber (Expr.Num (-7))))
  ]

arithmeticCases :: [(Expr.Expr, Either Error.Error Double)]
arithmeticCases =
  [ (Expr.Plus (Expr.Num 2) (Expr.Num 4), Right 6),
    (Expr.Plus (Expr.Num (-5)) (Expr.Num 5), Right 0),
    (Expr.Plus (Expr.Num (-2)) (Expr.Num 5), Right 3),
    (Expr.Plus (Expr.Div (Expr.Num 12) (Expr.Num 4)) (Expr.Num 3), Right 6),
    (Expr.Minus (Expr.Num 5) (Expr.Num 3), Right 2),
    (Expr.Minus (Expr.Num 2) (Expr.Num 5), Right (-3)),
    (Expr.Multi (Expr.Num 2) (Expr.Num 10), Right 20),
    (Expr.Multi (Expr.Num 2) (Expr.Num (-10)), Right (-20)),
    (Expr.Div (Expr.Num 24) (Expr.Num 6), Right 4),
    (Expr.Div (Expr.Num 145) (Expr.Num 0), Left (Error.DivisionByZero (Expr.Num 145) (Expr.Num 0))),
    (Expr.Power (Expr.Num 2) (Expr.Num 5), Right 32),
    (Expr.Div (Expr.Num 20) (Expr.Plus (Expr.Num 4) (Expr.Num (-4))), Left (Error.DivisionByZero (Expr.Num 20) (Expr.Plus (Expr.Num 4) (Expr.Num (-4)))))
  ]

letCases :: [(Expr.Expr, Either Error.Error Double)]
letCases =
  [ (Expr.Let "a" (Expr.Num 7) (Expr.Var "a"), Right 7),
    (Expr.Let "b" (Expr.Num 13) (Expr.Plus (Expr.Var "b") (Expr.Num 90)), Right 103),
    (Expr.Let "c" (Expr.Num 89) (Expr.Minus (Expr.Var "c") (Expr.Var "c")), Right 0),
    (Expr.Let "d" (Expr.Num 25) (Expr.Sqrt (Expr.Var "d")), Right 5),
    (Expr.Let "e" (Expr.Num 4) (Expr.Div (Expr.Num 100) (Expr.Var "e")), Right 25),
    (Expr.Let "f" (Expr.Num 10) (Expr.Let "f" (Expr.Num 4) (Expr.Var "f")), Right 4),
    (Expr.Let "g" (Expr.Num 20) (Expr.Let "x" (Expr.Num 50) (Expr.Plus (Expr.Var "g") (Expr.Var "x"))), Right 70),
    (Expr.Let "h" (Expr.Num 10) (Expr.Plus (Expr.Var "h") (Expr.Num 2)), Right 12),
    (Expr.Let "i" (Expr.Num 169) (Expr.Sqrt (Expr.Var "i")), Right 13),
    (Expr.Let "j" (Expr.Num (-25)) (Expr.Sqrt (Expr.Var "j")), Left (Error.SqrtOfNegNumber (Expr.Var "j"))),
    (Expr.Let "k" (Expr.Num 0) (Expr.Div (Expr.Num 25) (Expr.Var "k")), Left (Error.DivisionByZero (Expr.Num 25) (Expr.Var "k"))),
    (Expr.Let "l" (Expr.Num 25) (Expr.Div (Expr.Var "l") (Expr.Num 0)), Left (Error.DivisionByZero (Expr.Var "l") (Expr.Num 0)))
  ]

testExpr :: (Expr.Expr, Either Error.Error Double) -> TestTree
testExpr (expr, expected) =
  testCase (printf "Testing: %s" (show expr)) $
    let actual = Interpreter.eval Map.empty expr
     in case (expected, actual) of
          (Right expVal, Right actVal) -> assertEqual "" expVal actVal
          (Left expErr, Left actErr) -> assertEqual "" expErr actErr
          _ -> assertFailure $ printf "Expected %s but got %s" (show expected) (show actual)

numTests :: TestTree
numTests = testGroup "Numerical Expressions Tests" $ map testExpr numCases

arithmeticTests :: TestTree
arithmeticTests = testGroup "Arithmetic Operations Tests" $ map testExpr arithmeticCases

letTests :: TestTree
letTests = testGroup "Let Expressions Tests" $ map testExpr letCases

main :: IO ()
main = defaultMain $ testGroup "Expression Tests" [numTests, arithmeticTests, letTests]