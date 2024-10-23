import Test.Tasty
import Test.Tasty.HUnit
import Interpreter
import Expr
import Error
import qualified Data.Map as Map

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Interpreter Tests"
  [ testGroup "Number"
      [ testCase "Evaluate Number" $
          eval Map.empty (Number 5) @?= Right 5
      ]

  , testGroup "Variable"
      [ testCase "Evaluate Existing Variable" $
          eval (Map.fromList [("x", 10)]) (Var "x") @?= Right 10

      , testCase "Evaluate Unknown Variable" $
          eval Map.empty (Var "y") @?= Left (Unknownvariable "y")
      ]

  , testGroup "SquareRoot"
      [ testCase "Square Root of Positive" $
          eval Map.empty (SquareRoot (Number 9)) @?= Right 3

      , testCase "Square Root of Zero" $
          eval Map.empty (SquareRoot (Number 0)) @?= Right 0

      , testCase "Square Root of Negative" $
          eval Map.empty (SquareRoot (Number (-1))) @?= Left (NegativeSqrt (Number (-1)))
      ]

  , testGroup "Addition"
      [ testCase "Add Two Numbers" $
          eval Map.empty (Add (Number 2) (Number 3)) @?= Right 5

      , testCase "Add Variable and Number" $
          eval (Map.fromList [("x", 7)]) (Add (Var "x") (Number 3)) @?= Right 10
      ]

  , testGroup "Subtraction"
      [ testCase "Subtract Two Numbers" $
          eval Map.empty (Subtract (Number 5) (Number 3)) @?= Right 2

      , testCase "Subtract Variable and Number" $
          eval (Map.fromList [("x", 5)]) (Subtract (Var "x") (Number 2)) @?= Right 3
      ]

  , testGroup "Multiplication"
      [ testCase "Multiply Two Numbers" $
          eval Map.empty (Multiply (Number 4) (Number 5)) @?= Right 20

      , testCase "Multiply Variable and Number" $
          eval (Map.fromList [("x", 3)]) (Multiply (Var "x") (Number 4)) @?= Right 12
      ]

  , testGroup "Division"
      [ testCase "Divide Two Numbers" $
          eval Map.empty (Divide (Number 10) (Number 2)) @?= Right 5

      , testCase "Divide by Zero" $
          eval Map.empty (Divide (Number 10) (Number 0)) @?= Left (DivisionByZero (Number 0))
      ]

  , testGroup "Power"
      [ testCase "Power of Two Numbers" $
          eval Map.empty (Power (Number 2) (Number 3)) @?= Right 8

      , testCase "Power with Zero" $
          eval Map.empty (Power (Number 0) (Number 0)) @?= Right 1
      ]
  ]