import Test.Tasty
import Test.Unit
import Test.Prop
import Hedgehog
import qualified Expr
import HW.Compiler
import HW.Eval

main :: IO ()
main =
  defaultMain $ testGroup "All Tests" [
      testGroup "Unit" unitTests
      , testGroup "Props" props
    ]
-- main = print $ execProgram (compile $ Expr.Let "aaaaaaaaaaa" (Expr.Num 1) (Expr.Var "ooooooooooo")) initialState
