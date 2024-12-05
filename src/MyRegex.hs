{-# LANGUAGE InstanceSigs #-}
module MyRegex (
    MyRegex(..),
     RegexError(..),
    CharClass(..),
    mkCharClass,
    mkLiteral,
    mkPlus,
    mkQuestion,
    eval,
    splits,
    matchCharClass
    ) where

import Data.Char (isDigit, isLower, isUpper, isAlpha)

-- | Basic error types for regex operations
data RegexError = 
    EmptyRegex
  | InvalidCharClass String
  | EmptyCharClass
  | InvalidRange Char Char
  | UnexpectedEnd
  | InvalidEscape String
  | InvalidOperator String
  deriving (Show, Eq)


-- | Character class definitions
data CharClass = 
    SingleChar Char
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

-- | Core regex pattern types
data MyRegex = 
    Literal Char
  | CharClassMatch CharClass
  | Disjunction MyRegex MyRegex  -- a|b
  | Concat MyRegex MyRegex       -- ab
  | Star MyRegex                 -- a*
  | Plus MyRegex                 -- a+
  | Question MyRegex             -- a?
  deriving (Eq)

instance Show MyRegex where
  show :: MyRegex -> String
  show (Literal a) = [a]
  show (CharClassMatch cc) = show cc
  show (Disjunction a b) = "(" ++ show a ++ "|" ++ show b ++ ")"
  show (Concat a b) = show a ++ show b
  show (Star a) = "(" ++ show a ++ ")*"
  show (Plus a) = "(" ++ show a ++ ")+"
  show (Question a) = "(" ++ show a ++ ")?"

-- | Smart constructors
mkLiteral :: Char -> Either RegexError MyRegex
mkLiteral = Right . Literal

mkCharClass :: CharClass -> Either RegexError MyRegex
mkCharClass cc = case validateCharClass cc of
  Nothing -> Right $ CharClassMatch cc
  Just err -> Left err

mkPlus :: MyRegex -> Either RegexError MyRegex
mkPlus = Right . Plus

mkQuestion :: MyRegex -> Either RegexError MyRegex
mkQuestion = Right . Question

-- | Validation helpers
validateCharClass :: CharClass -> Maybe RegexError
validateCharClass (CharRange start end)
  | end < start = Just $ InvalidRange start end
  | otherwise = Nothing
validateCharClass _ = Nothing

-- | Character matching
matchCharClass :: CharClass -> Char -> Bool
matchCharClass (SingleChar c) x = c == x
matchCharClass (CharRange start end) x = x >= start && x <= end
matchCharClass Digit x = isDigit x
matchCharClass Alpha x = isAlpha x
matchCharClass Lower x = isLower x
matchCharClass Upper x = isUpper x

-- | String splitting helper
splits :: String -> [(String, String)]
splits s = zip (inits s) (tails s)
  where
    inits [] = [[]]
    inits (x:xs) = [] : map (x:) (inits xs)
    tails [] = [[]]
    tails (x:xs) = (x:xs) : tails xs

-- | Pattern matching
eval :: MyRegex -> String -> Bool
eval (Literal c) [x] = c == x
eval (CharClassMatch cc) [x] = matchCharClass cc x
eval (Disjunction a b) s = eval a s || eval b s
eval (Concat a b) s = any (\(s1, s2) -> eval a s1 && eval b s2) (splits s)
eval (Star r) s = 
  s == "" || any (\(pre, suf) -> pre /= "" && eval r pre && eval (Star r) suf) (splits s)
eval (Plus r) s = 
  any (\(pre, suf) -> pre /= "" && eval r pre && eval (Star r) suf) (splits s)
eval (Question r) s = s == "" || eval r s
eval _ _ = False