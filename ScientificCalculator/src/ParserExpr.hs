module ParserExpr where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import Control.Monad.Identity

import Expr


lexer = Token.makeTokenParser emptyDef
parens = Token.parens lexer
integer = Token.integer lexer
float = Token.float lexer
reservedOp = Token.reservedOp lexer


parseNumber :: Parser Expr
parseNumber = do
  sign <- optionMaybe (char '-')
  n <- try float <|> (fromIntegral <$> integer)
  let value = maybe n (const (-n)) sign
  return (Num value)


parseParens :: Parser Expr
parseParens = parens parseExpr


parseConst :: Parser Expr
parseConst = 
      (reservedOp "pi" >> return Pi)
  <|> (reservedOp "π" >> return Pi)
  <|> (reservedOp "e" >> return E)



operatorTable :: [[Operator String () Identity Expr]]
operatorTable =
  [ [ Postfix (reservedOp "!" >> return Factorial) ]
  , [ Prefix (reservedOp "rad" >> return Rad)
    , Prefix (reservedOp "deg" >> return Deg)
    , Prefix (reservedOp "sqrt" >> return Sqrt)
    , Prefix (reservedOp "cbrt" >> return Cbrt)
    , Prefix (reservedOp "1/x" >> return Reciprocal)
    , Prefix (reservedOp "x^2" >> return Square)
    , Prefix (reservedOp "x^3" >> return Cube)
    , Prefix (reservedOp "ln" >> return Ln)
    , Prefix (reservedOp "log10" >> return Log10)
    , Prefix (reservedOp "exp" >> return Exp)
    , Prefix (reservedOp "asinh" >> return ASinh)
    , Prefix (reservedOp "acosh" >> return ACosh)
    , Prefix (reservedOp "atanh" >> return ATanh)
    , Prefix (reservedOp "asin" >> return ASin)
    , Prefix (reservedOp "acos" >> return ACos)
    , Prefix (reservedOp "atan" >> return ATan)
    , Prefix (reservedOp "sinh" >> return Sinh)
    , Prefix (reservedOp "cosh" >> return Cosh)
    , Prefix (reservedOp "tanh" >> return Tanh)
    , Prefix (reservedOp "sin" >> return Sin)
    , Prefix (reservedOp "cos" >> return Cos)
    , Prefix (reservedOp "tan" >> return Tan)
    ]
  , [ Infix (reservedOp "^" >> return Pow) AssocRight
    , Infix (reservedOp "Lny" >> return LogBase) AssocRight
    , Infix (reservedOp "EE" >> return Exp10) AssocRight
    ]
  , [ Infix (reservedOp "*" >> return Mul) AssocLeft
    , Infix (reservedOp "/" >> return Div) AssocLeft
    ]
  , [ Infix (reservedOp "+" >> return Add) AssocLeft
    , Infix (reservedOp "-" >> return Diff) AssocLeft
    ]
  ]


processString :: String -> String
processString [] = []
processString ('d':'e':'g':t) = "deg" ++ processString t
processString ('e':'x':'p':t) = "exp" ++ processString t 
processString ('e':t) = "e " ++ processString t
processString ('π':t) = "π " ++ processString t
processString ('p':'i':t) =  "pi " ++ processString t
processString (x:t) = x : processString t


parseExpr :: Parser Expr
parseExpr = buildExpressionParser operatorTable parseTerm <?> "Invalid expression"


parseTerm :: Parser Expr
parseTerm = parseNumber 
        <|> parseParens 
        <|> parseConst 
        <?> "Expected a valid term (number, constant, or parentheses)"



parseInput :: String -> Either ParseError Expr
parseInput input = do
  let exprInput = processString input
  parse (parseExpr <* eof) "" exprInput 



unaryOperator :: Parser (Expr -> Expr)
unaryOperator =
      (reservedOp "rad" >> return Rad)
  <|> (reservedOp "!" >> return Factorial)
  <|> (reservedOp "deg" >> return Deg)
  <|> (reservedOp "sqrt" >> return Sqrt)
  <|> (reservedOp "cbrt" >> return Cbrt)
  <|> (reservedOp "1/x" >> return Reciprocal)
  <|> (reservedOp "x^2" >> return Square)
  <|> (reservedOp "x^3" >> return Cube)
  <|> (reservedOp "ln" >> return Ln)
  <|> (reservedOp "log10" >> return Log10)
  <|> (reservedOp "exp" >> return Exp)
  <|> (reservedOp "asinh" >> return ASinh)
  <|> (reservedOp "acosh" >> return ACosh)
  <|> (reservedOp "atanh" >> return ATanh)
  <|> (reservedOp "asin" >> return ASin)
  <|> (reservedOp "acos" >> return ACos)
  <|> (reservedOp "atan" >> return ATan)
  <|> (reservedOp "sinh" >> return Sinh)
  <|> (reservedOp "cosh" >> return Cosh)
  <|> (reservedOp "tanh" >> return Tanh)
  <|> (reservedOp "sin" >> return Sin)
  <|> (reservedOp "cos" >> return Cos)
  <|> (reservedOp "tan" >> return Tan)


binaryOperator :: Parser (Expr -> Expr -> Expr)
binaryOperator =
      (reservedOp "+" >> return Add)
  <|> (reservedOp "-" >> return Diff)
  <|> (reservedOp "*" >> return Mul)
  <|> (reservedOp "/" >> return Div)
  <|> (reservedOp "^" >> return Pow)
  <|> (reservedOp "Lny" >> return LogBase)
  <|> (reservedOp "EE" >> return Exp10)


operationParser :: Expr -> Parser Expr
operationParser lastResult = do
  spaces
  opUnary <- optionMaybe (try unaryOperator)
  case opUnary of
    Just unary -> do
      let resultUnary = unary lastResult
      spaces
      opBinary <- optionMaybe (try binaryOperator)
      case opBinary of
        Just binary -> do 
          spaces
          binary resultUnary <$> parseExpr
        Nothing -> return resultUnary
    Nothing -> do
      opBinary <- optionMaybe (try binaryOperator)
      case opBinary of
        Just binary -> do 
          spaces
          binary lastResult <$> parseExpr
        Nothing -> do
          spaces
          parseExpr

parseContinuation :: Expr -> String -> Either ParseError Expr
parseContinuation lastResult input = do
  let processedInput = processString input
  parse (operationParser lastResult <* eof) "" processedInput
