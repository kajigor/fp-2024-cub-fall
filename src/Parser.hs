module Parser (
    Expr(..),
    parseExpr,
    parseNumber
) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Control.Monad.Identity (Identity)

-- expression types
data Expr 
    = Num Double 
    | Var String 
    | Add Expr Expr 
    | Sub Expr Expr 
    | Mul Expr Expr 
    | Div Expr Expr 
    | Pow Expr Expr
    | Sqrt Expr
    | Neg Expr
    | Sin Expr
    | Cos Expr
    | Tan Expr
    | Log Expr
    | Ln Expr
    | Pi
    | E
    deriving (Show)

-- lexer definition
lexer :: TokenParser ()
lexer = makeTokenParser emptyDef {
    reservedNames = ["sin", "cos", "tan", "log", "ln", "pi", "e", "sqrt"]
}

-- parsing functions
parseExpr :: Parser Expr
parseExpr = spaces *> buildExpressionParser operators parseFactor <* spaces <?> "expression"

operators :: OperatorTable String () Identity Expr
operators = [
    [Prefix (reservedOp lexer "-" >> return Neg)],
    [Infix (reservedOp lexer "^" >> return Pow) AssocRight],
    [Infix (reservedOp lexer "*" >> return Mul) AssocLeft,
     Infix (reservedOp lexer "/" >> return Div) AssocLeft],
    [Infix (reservedOp lexer "+" >> return Add) AssocLeft,
     Infix (reservedOp lexer "-" >> return Sub) AssocLeft]
  ]

-- parsing trig and log functions
parseFunc :: Parser Expr
parseFunc = do
    func <- choice $ map (try . string) ["sin", "cos", "tan", "log", "ln", "sqrt"]
    spaces
    expr <- parseFactor
    return $ case func of
        "sin" -> Sin expr
        "cos" -> Cos expr
        "tan" -> Tan expr
        "log" -> Log expr
        "sqrt" -> Sqrt expr
        "ln" -> Ln expr
        _ -> error ("Unknown function: " ++ func)

parseConstant :: Parser Expr
parseConstant = (string "pi" >> return Pi) <|> (string "e" >> return E)

parseNumber :: Parser Expr
parseNumber = (Num . read) <$> many1 (digit <|> char '.' <|> oneOf "-+eE")

parseFactor :: Parser Expr
parseFactor = spaces *> (parseConstant
           <|> parseFunc
           <|> parseVariable 
           <|> parseNumber
           <|> parens lexer parseExpr) <* spaces

parseVariable :: Parser Expr
parseVariable = Var <$> many1 letter <?> "variable"
