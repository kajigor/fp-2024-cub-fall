module Parser (
    parse
) where

import MyRegex

data Parser = Parser {
    input :: String,
    pos :: Int
}

data ParseResult a = Success Parser a
                   | Failure RegexError

initParser :: String -> Parser
initParser s = Parser s 0

current :: Parser -> Maybe Char
current p 
    | pos p >= length (input p) = Nothing
    | otherwise = Just $ input p !! pos p

advance :: Parser -> Parser
advance p = p { pos = pos p + 1 }

parseRegex :: String -> Either RegexError MyRegex
parseRegex "" = Left EmptyRegex
parseRegex s = case parseExpr (initParser s) of
    Success p r -> if pos p == length (input p)
                  then Right r 
                  else Left $ InvalidOperator "Unexpected characters at end"
    Failure err -> Left err

parseExpr :: Parser -> ParseResult MyRegex
parseExpr p = case parseTerm p of
    Success p1 left -> case current p1 of
        Just '|' -> case parseExpr (advance p1) of
            Success p2 right -> Success p2 (Disjunction left right)
            Failure err -> Failure err
        _ -> Success p1 left
    Failure err -> Failure err

parseTerm :: Parser -> ParseResult MyRegex
parseTerm p = case parseUnary p of
    Success p1 first -> case current p1 of
        Nothing -> Success p1 first
        Just c | c `elem` "|" -> Success p1 first
              | otherwise -> case parseTerm p1 of
                    Success p2 rest -> Success p2 (Concat first rest)
                    Failure err -> Failure err
    Failure err -> Failure err

parseUnary :: Parser -> ParseResult MyRegex
parseUnary p = case parseAtom p of
    Success p1 atom -> case current p1 of 
        Just '*' -> Success (advance p1) (Star atom)
        Just '+' -> Success (advance p1) (Plus atom)
        Just '?' -> Success (advance p1) (Question atom)
        _ -> Success p1 atom
    Failure err -> Failure err

parseAtom :: Parser -> ParseResult MyRegex
parseAtom p = case current p of
    Nothing -> Failure UnexpectedEnd
    Just c -> case c of
        '\\' -> parseEscape (advance p)
        '*' -> Failure $ InvalidOperator "Unexpected *"
        '+' -> Failure $ InvalidOperator "Unexpected +"
        '?' -> Failure $ InvalidOperator "Unexpected ?"
        '|' -> Failure $ InvalidOperator "Unexpected |"
        _ -> Success (advance p) (Literal c)

parseEscape :: Parser -> ParseResult MyRegex
parseEscape p = case current p of
    Nothing -> Failure $ InvalidOperator "Unexpected end after escape"
    Just c -> case c of
        'd' -> case mkCharClass Digit of
                Right r -> Success (advance p) r
                Left err -> Failure err
        'w' -> case mkCharClass Alpha of
                Right r -> Success (advance p) r
                Left err -> Failure err
        'l' -> case mkCharClass Lower of
                Right r -> Success (advance p) r
                Left err -> Failure err
        'u' -> case mkCharClass Upper of
                Right r -> Success (advance p) r
                Left err -> Failure err
        _ -> Success (advance p) (Literal c)

parse :: String -> Either RegexError MyRegex
parse = parseRegex
