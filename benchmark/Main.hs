import Criterion.Main
import qualified Data.Map as M
import Hedgehog (Gen, (===), property, forAll, success, PropertyT, Property, TestLimit, withTests)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Expr
import Eval
import Parser
import TestUtil (genExpr)
import System.Random (StdGen, mkStdGen, randomR)
import Expr

generateComplexExpr :: Int -> Expr
generateComplexExpr complexity =
    let gen = mkStdGen complexity in
        fst $ buildExpr complexity gen ["x", "y", "z"]

buildExpr :: Int -> StdGen -> [String] -> (Expr, StdGen)
buildExpr complexity gen vars
    | complexity <= 1 = randomLeaf gen vars
    | otherwise =
        let (op, gen1) = randomOp gen
            (leftComplexity, gen2) = randomR (1, complexity - 1) gen1
            rightComplexity = complexity - leftComplexity
            (leftExpr, gen3) = buildExpr leftComplexity gen2 vars
            (rightExpr, gen4) = buildExpr rightComplexity gen3 vars
        in (applyOp op leftExpr rightExpr, gen4)

randomLeaf :: StdGen -> [String] -> (Expr, StdGen)
randomLeaf gen vars =
    let (choice, gen1) = randomR (0, 1 :: Int) gen
    in case choice of
         0 -> let (num, gen2) = randomR (-1000.0, 1000.0) gen1
              in (Num num, gen2)
         1 -> let (varIdx, gen2) = randomR (0, length vars - 1) gen1
              in (Var (vars !! varIdx), gen2)

randomOp :: StdGen -> (String, StdGen)
randomOp gen =
    let (opIdx, gen1) = randomR (0, 6 :: Int) gen
        ops = ["+", "-", "*", "/", "^", "abs", "unaryMinus"]
    in (ops !! opIdx, gen1)

applyOp :: String -> Expr -> Expr -> Expr
applyOp "+" e1 e2 = Plus e1 e2
applyOp "-" e1 e2 = Minus e1 e2
applyOp "*" e1 e2 = Mult e1 e2
applyOp "/" e1 e2 = Div e1 e2
applyOp "^" e1 e2 = Pow e1 e2
applyOp "abs" e1 _ = Abs e1
applyOp "unaryMinus" e1 _ = UnaryMinus e1
applyOp _ _ _ = error "Invalid operator"

main :: IO ()
main = do
    let complexities = [20, 40, 80, 160]
        varMap = M.fromList [("x", 1.0), ("y", 2.0), ("z", 3.0)]
        exprs = [(c, generateComplexExpr c) | c <- complexities]

    defaultMain
        [ bgroup "Parse Benchmarks"
            [ bench ("parseExpr, complexity " ++ show c) $ whnf (parseExpr . show) expr
            | (c, expr) <- exprs
            ]
        , bgroup "Eval Benchmarks"
            [ bench ("evalExpr, complexity " ++ show c) $ whnf (evalExpr varMap) expr
            | (c, expr) <- exprs
            ]
        ]
