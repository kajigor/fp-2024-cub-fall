{-# LANGUAGE InstanceSigs #-}
module MyRegex (
    MyRegex(..),
    RegexError(..),
    CharClass(..),
    matchCharClass,
    splits,
    eval,
    mkLiteral,
    mkCharClass,
    mkPlus,
    mkQuestion
) where

import Data.Char (isDigit, isLower, isUpper, isAlpha)

data RegexError = EmptyRegex
                | InvalidCharClass String
                | EmptyCharClass
                | InvalidRange Char Char
                | UnmatchedParenthesis
                | InvalidEscape String
                | InvalidOperator String
                | UnexpectedEnd
                deriving (Show, Eq)

data CharClass = SingleChar Char
              | CharRange Char Char
              | Digit
              | Alpha
              | Lower
              | Upper
              deriving (Eq)

instance Show CharClass where
    show (SingleChar c) = [c]
    show (CharRange start end) = ['[', start, '-', end, ']']
    show Digit = "\\d"
    show Alpha = "[a-zA-Z]"
    show Lower = "[a-z]"
    show Upper = "[A-Z]"

data MyRegex = Literal Char
             | CharClassMatch CharClass
             | Disjunction MyRegex MyRegex
             | Concat MyRegex MyRegex
             | Star MyRegex
             | Plus MyRegex
             | Question MyRegex
             deriving (Eq)

instance Show MyRegex where
    show :: MyRegex -> String
    show (Literal a) = show a
    show (CharClassMatch cc) = show cc
    show (Disjunction a b) = "(" ++ show a ++ "|" ++ show b ++ ")"
    show (Concat a b) = show a ++ show b
    show (Star a) = "(" ++ show a ++ ")*"
    show (Plus a) = "(" ++ show a ++ ")+"
    show (Question a) = "(" ++ show a ++ ")?"

mkLiteral :: Char -> Either RegexError MyRegex
mkLiteral c = Right $ Literal c

mkCharClass :: CharClass -> Either RegexError MyRegex
mkCharClass cc = case validateCharClass cc of
    Nothing -> Right $ CharClassMatch cc
    Just err -> Left err

mkPlus :: MyRegex -> Either RegexError MyRegex
mkPlus r = Right $ Plus r

mkQuestion :: MyRegex -> Either RegexError MyRegex
mkQuestion r = Right $ Question r

validateCharClass :: CharClass -> Maybe RegexError
validateCharClass (CharRange start end)
    | end < start = Just $ InvalidRange start end
    | otherwise = Nothing
validateCharClass _ = Nothing

matchCharClass :: CharClass -> Char -> Bool
matchCharClass (SingleChar c) x = c == x
matchCharClass (CharRange start end) x = x >= start && x <= end
matchCharClass Digit x = isDigit x
matchCharClass Alpha x = isAlpha x
matchCharClass Lower x = isLower x
matchCharClass Upper x = isUpper x

splits :: String -> [(String, String)]
splits s = zip (inits s) (tails s)
    where 
        inits [] = [[]]
        inits (x:xs) = [] : map (x:) (inits xs)
        
        tails [] = [[]]
        tails (x:xs) = (x:xs) : tails xs

evalHelper :: MyRegex -> String -> String -> Bool
evalHelper r@(Concat a b) s1@(_:_) s2 = 
    (eval a s1 && eval b s2) || evalHelper r (init s1) (last s1:s2)
evalHelper _ _ _ = False

eval :: MyRegex -> String -> Bool
eval (Literal c) [x] = c == x
eval (Literal _) _ = False
eval (CharClassMatch cc) [x] = matchCharClass cc x
eval (CharClassMatch _) _ = False
eval (Disjunction a b) s = eval a s || eval b s
eval r@(Concat _ _) s = evalHelper r s ""
eval (Star r) s = 
    s == "" || 
    any (\(prefix, suffix) -> 
        prefix /= "" &&
        eval r prefix &&
        eval (Star r) suffix)
        (splits s)
eval (Plus r) s = 
    any (\(prefix, suffix) -> 
        prefix /= "" &&
        eval r prefix &&
        eval (Star r) suffix)
        (splits s)
eval (Question r) s =
    s == "" || eval r s

