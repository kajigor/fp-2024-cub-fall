module Parser (
    parse
) where

import MyRegex ( MyRegex(..), CharClass(..), RegexError(..), mkCharClass)

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
        Just c | c `elem` ")|" -> Success p1 first
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
        '(' -> parseGroup (advance p)
        '[' -> parseCharClass (advance p)
        '\\' -> parseEscape (advance p)
        '*' -> Failure $ InvalidOperator "Unexpected *"
        '+' -> Failure $ InvalidOperator "Unexpected +"
        '?' -> Failure $ InvalidOperator "Unexpected ?"
        '|' -> Failure $ InvalidOperator "Unexpected |"
        ')' -> Failure $ InvalidOperator "Unexpected )"
        ']' -> Failure $ InvalidOperator "Unexpected ]"
        _ -> Success (advance p) (Literal c)

parseGroup :: Parser -> ParseResult MyRegex
parseGroup p = case parseExpr p of
    Success p1 expr -> case current p1 of
        Just ')' -> Success (advance p1) expr
        _ -> Failure $ InvalidOperator "Expected closing parenthesis"
    Failure err -> Failure err

parseCharClass :: Parser -> ParseResult MyRegex
parseCharClass p = case current p of
    Nothing -> Failure UnexpectedEnd
    Just first -> case advance p of
        p1 -> case current p1 of
            Just '-' -> case current (advance p1) of
                Nothing -> Failure UnexpectedEnd
                Just lst -> if lst < first
                            then Failure $ InvalidRange first lst
                            else case current (advance (advance p1)) of
                                Just ']' -> case mkCharClass (CharRange first lst) of
                                    Right r -> Success (advance (advance (advance p1))) r
                                    Left err -> Failure err
                                _ -> Failure $ InvalidOperator "Expected closing bracket"
            _ -> Failure $ InvalidOperator "Expected range operator"

parseEscape :: Parser -> ParseResult MyRegex
parseEscape p = case current p of
    Nothing -> Failure $ InvalidEscape "Unexpected end after escape"
    Just c -> case c of
        'd' -> mkCharClassMatch Digit p
        'a' -> mkCharClassMatch Alpha p
        'l' -> mkCharClassMatch Lower p
        'u' -> mkCharClassMatch Upper p
        _ -> Failure $ InvalidEscape [c]
    where
        mkCharClassMatch cc pp = case mkCharClass cc of
            Right r -> Success (advance pp) r
            Left err -> Failure err

parse :: String -> Either RegexError MyRegex
parse = parseRegex