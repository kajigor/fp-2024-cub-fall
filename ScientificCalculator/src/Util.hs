module Util where

import System.Console.Haskeline

helpString :: [String]
helpString =
  [ "Scientific Calculator Help"
  , ""
  , "Supported Commands:"
  , "  =       : Evaluate an expression."
  , "  ac      : Clear the last result (sets it to 0.0)."
  , "  m+      : Add the last result to memory."
  , "  m-      : Subtract the last result from memory."
  , "  mc      : Clear the memory (sets it to 0.0)."
  , "  mr      : Retrieve the value in memory."
  , "  help    : Show this help message."
  , "  quit/q  : Exit the calculator."
  , ""
  , "Supported Operations:"
  , "  Unary Functions:"
  , "    sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh"
  , "    sqrt, cbrt, ln, log10, exp, deg, rad, 1/x, x^2, x^3, ! (factorial)"
  , "  Binary Operators:"
  , "    +, -, *, /, ^ (power), Lny (logarithm with custom base), EE (scientific notation)"
  , "  Constants:"
  , "    π (pi), e (Euler's number)"
  , ""
  , "Notes:"
  , "  - Trigonometric functions expect angles in radians by default."
  , "  - Parentheses are important to enforce precedence."
  , "  - Enter 'help' to view this help message at any time."
  , ""
  , "Examples:"
  , "  > ="
  , "  Enter an expression to evaluate:"
  , "  sin (π / 2)"
  , "  Result: 1.0"
  , "  > m+"
  , "  Memory: 1.0"
  , "  > + 9"
  , "  Result: 10.0"
  ]


getUserInput :: IO String
getUserInput = runInputT defaultSettings { historyFile = Just ".calculator_history" } $ do
    line <- getInputLine ""
    case line of
      Nothing -> return ""
      Just value -> return value


removeTrailingSpaces :: String -> String
removeTrailingSpaces (' ':tail) = removeTrailingSpaces tail
removeTrailingSpaces str = str