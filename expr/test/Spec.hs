import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import Error
import Interpreter(eval)
import Expr
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

testEval :: TestTree
testEval =
  testGroup "Eval" [testArithmetic, testVariables, testLet, testErrors]
  where
    testEvalSuccess msg state expr res =
      testCase msg $ eval state expr @?= Right res
    
    testEvalFailure msg state expr expectedError =
      testCase msg $ eval state expr @?= Left expectedError

    -- Test basic arithmetic expressions
    testArithmetic =
      testGroup
        "Arithmetic"
        [ testEvalSuccess "1 + 2 == 3" M.empty (Add (Number 1) (Number 2)) 3,
          testEvalSuccess "5 - 3 == 2" M.empty (Sub (Number 5) (Number 3)) 2,
          testEvalSuccess "4 * 2 == 8" M.empty (Mult (Number 4) (Number 2)) 8,
          testEvalSuccess "9 / 3 == 3" M.empty (Div (Number 9) (Number 3)) 3,
          testEvalSuccess "2 ^ 3 == 8" M.empty (Exp (Number 2) (Number 3)) 8,
          testEvalSuccess "sqrt(16) == 4" M.empty (Sqrt (Number 16)) 4
        ]
    
    -- Test variable evaluation
    testVariables =
      testGroup
        "Variables"
        [ testEvalSuccess "x = 5" (M.singleton "x" 5) (Var "x") 5,
          testEvalFailure "x not found" M.empty (Var "x") (NotVariable "x")
        ]

    -- Test Let expressions
    testLet =
      testGroup
        "Let"
        [ testEvalSuccess "let x = 5 in x + 1 == 6"
            M.empty
            (Let "x" (Number 5) (Add (Var "x") (Number 1)))
            6,
          testEvalSuccess "nested let: let x = 5 in let y = x + 1 in y * 2 == 12"
            M.empty
            (Let "x" (Number 5) (Let "y" (Add (Var "x") (Number 1)) (Mult (Var "y") (Number 2))))
            12
        ]

    -- Test error cases
    testErrors =
      testGroup
        "Errors"
        [ testEvalFailure "division by zero" M.empty (Div (Number 4) (Number 0)) (DividedByZero (Div (Number 4) (Number 0))),
          testEvalFailure "sqrt of negative number" M.empty (Sqrt (Number (-1))) (SqrtOfNegative (Sqrt (Number (-1)))),
          testEvalFailure "negative exponent" M.empty (Exp (Number 2) (Number (-1))) (NegativeExponent (Exp (Number 2) (Number (-1)))),
          testEvalFailure "multiple declaration of x" M.empty (Let "x" (Number 5) (Let "x" (Number 10) (Var "x"))) (MultipleDeclaration "x")
        ]

main :: IO ()
main =
  defaultMain $ testGroup "Expressions" [testEval]
