import qualified Data.Map as M
import Expr (Expr (..))
import qualified HW.Compiler as Compiler
import qualified HW.Eval as Eval
import qualified HW.StackMachine as StackMachine
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)

compilationCases :: [(Expr String, [StackMachine.StackInstr String])]
compilationCases =
  [ (Let "x" (Num 13) (Var "x"), [StackMachine.PushNum 13, StackMachine.StoreVar "x", StackMachine.PushVar "x"]),
    (Plus (Num 10) (Num 5), [StackMachine.PushNum 10, StackMachine.PushNum 5, StackMachine.Add]),
    ( Let "x" (Num 7) (Plus (Var "x") (Num 3)),
      [StackMachine.PushNum 7, StackMachine.StoreVar "x", StackMachine.PushVar "x", StackMachine.PushNum 3, StackMachine.Add]
    ),
    ( Let "x" (Num 10) (Let "y" (Num 20) (Plus (Var "x") (Var "y"))),
      [StackMachine.PushNum 10, StackMachine.StoreVar "x", StackMachine.PushNum 20, StackMachine.StoreVar "y", StackMachine.PushVar "x", StackMachine.PushVar "y", StackMachine.Add]
    ),
    ( Let "x" (Num 5) (Let "x" (Num 10) (Plus (Var "x") (Num 20))),
      [StackMachine.PushNum 5, StackMachine.StoreVar "x", StackMachine.PushNum 10, StackMachine.StoreVar "x", StackMachine.PushVar "x", StackMachine.PushNum 20, StackMachine.Add]
    )
  ]

evaluationCases :: [([StackMachine.StackInstr String], Either (Eval.Error String) Int)]
evaluationCases =
  [ ([StackMachine.PushNum 13, StackMachine.StoreVar "x", StackMachine.PushVar "x"], Right 13),
    ([StackMachine.PushNum 10, StackMachine.PushNum 15, StackMachine.Add], Right 25),
    ([StackMachine.PushNum 25, StackMachine.StoreVar "x", StackMachine.PushVar "x", StackMachine.PushNum 16, StackMachine.Add], Right 41),
    ([StackMachine.PushNum 13, StackMachine.Add], Left (Eval.StackUnderflow StackMachine.Add)),
    ([StackMachine.PushVar "y"], Left (Eval.VarUndefined "\"y\"")),
    ([StackMachine.PushNum 5, StackMachine.StoreVar "x", StackMachine.PushNum 25, StackMachine.StoreVar "x", StackMachine.PushVar "x"], Right 25),
    ( [ StackMachine.PushNum 7,
        StackMachine.PushNum 3,
        StackMachine.Add,
        StackMachine.StoreVar "x",
        StackMachine.PushVar "x",
        StackMachine.PushNum 4,
        StackMachine.Add
      ],
      Right 14
    )
  ]

testCompilation :: (Expr String, [StackMachine.StackInstr String]) -> TestTree
testCompilation (expr, expected) =
  testCase
    (printf "Compiling: %s" (show expr))
    (let actual = Compiler.compile expr in assertEqual "" expected actual)

testEvaluation :: ([StackMachine.StackInstr String], Either (Eval.Error String) Int) -> TestTree
testEvaluation (instructions, expected) =
  testCase
    (printf "Evaluating program: %s" (show instructions))
    ( let actual = fmap Eval.getStack (Eval.execProgram instructions Eval.initialState)
       in assertEqual "" expected (fmap head actual)
    )

compilationTests :: TestTree
compilationTests = testGroup "Compilation Tests" (map testCompilation compilationCases)

evaluationTests :: TestTree
evaluationTests = testGroup "Evaluation Tests" (map testEvaluation evaluationCases)

main :: IO ()
main = defaultMain (testGroup "Stack Machine Tests" [compilationTests, evaluationTests])
