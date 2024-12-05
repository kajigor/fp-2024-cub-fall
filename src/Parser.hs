module Parser (parseExpr) where

import Expr
import Data.Functor.Identity
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as E
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Char as C
import Text.Parsec.Language (haskellDef)
import Text.ParserCombinators.Parsec.Prim (getInput)

type ExprParser = P.Parsec String () Expr

exprParser :: ExprParser
exprParser = E.buildExpressionParser table term
    P.<?> "arithmetic expression"

term :: P.ParsecT String () Identity Expr
term   =  parens exprParser
    P.<|> float
    P.<|> var
    P.<?> "simple arithmetic expression"

table :: E.OperatorTable String () Identity Expr
table = [ [prefix "-" UnaryMinus, prefix "+" id]
        , [prefix "abs" Abs]
        , [binary "**" Pow E.AssocRight]
        , [binary "*" Mult E.AssocLeft, binary "/" Div E.AssocLeft]
        , [binary "+" Plus E.AssocLeft, binary "-" Minus E.AssocLeft]
        ]

lexer :: T.GenTokenParser String () Identity
lexer = T.makeTokenParser haskellDef

parens :: P.ParsecT String () Identity a -> P.ParsecT String () Identity a
parens = T.parens lexer

float :: P.ParsecT String () Identity Expr
float = Num . toDouble <$> T.naturalOrFloat lexer

var :: P.ParsecT String () Identity Expr
var = Var <$> T.stringLiteral lexer

toDouble :: Either Integer Double -> Double
toDouble (Left x) = fromInteger x
toDouble (Right x) = x

parseOp :: String -> b -> P.ParsecT String () Identity b
parseOp name fun = do
    T.reservedOp lexer name
    return fun
binary :: String -> (a -> a -> a) -> E.Assoc -> E.Operator String () Identity a
binary name fun = E.Infix $ parseOp name fun

prefix :: String -> (a -> a) -> E.Operator String () Identity a
prefix name fun = E.Prefix $ parseOp name fun

parseExpr :: String -> Either P.ParseError Expr
parseExpr = P.runParser (exprParser <* P.eof) () ""
