import Criterion
import Criterion.Main

import Parser (parseExpr)

main :: IO ()
main = defaultMain [bench "1 + abs(2 - 3)" (whnf parseExpr "1 + abs(2 - 3)")]
