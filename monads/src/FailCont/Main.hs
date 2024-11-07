module FailCont.Main where

import FailCont.FailCont 
import Text.Read (readMaybe)

data Error 
  = EmptyInput 
  | ParseFailed String 
  | DivisionByZero
  deriving (Show, Eq)

defaultFunc :: Char -> String -> String -> FailCont r Error Int
defaultFunc _ "" _ = FailCont $ \e _ -> e EmptyInput
defaultFunc _ _ "" = FailCont $ \e _ -> e EmptyInput
defaultFunc op num1 num2 = FailCont $ \e s ->
  case (readMaybe num1 :: Maybe Int, readMaybe num2 :: Maybe Int) of
    (Just x, Just y) -> 
      case op of
        '+' -> s (x + y)
        '\\' -> if y == 0 then e DivisionByZero else s (x `div` y)
        'g' -> if y == 0 then s x else runFailCont (findGCD (show y) (show (x `mod` y))) e s
        'f' -> if (x >= 0 && y >= 0)
               then s (product [1..x] * product [1..y])
               else e (ParseFailed "Factorial of a negative number is undefined")
    (Nothing, _) -> e (ParseFailed num1)
    (_, Nothing) -> e (ParseFailed num2)

addInts :: String -> String -> FailCont r Error Int
addInts num1 num2 = defaultFunc '+' num1 num2

divInts :: String -> String -> FailCont r Error Int 
divInts num1 num2 = defaultFunc '\\' num1 num2

--Returns the greatest common divisor of two numbers
findGCD :: String -> String -> FailCont r Error Int
findGCD num1 num2 = defaultFunc 'g' num1 num2

--Returns the product of the factorials of two numbers
factorialMult :: String -> String -> FailCont r Error Int
factorialMult num1 num2 = defaultFunc 'f' num1 num2

main = do 
  print $ evalFailCont $ addInts "13" "42"         -- Right 55
  print $ evalFailCont $ addInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ addInts "13" "fourty two" -- Left (ParseFailed "fourty two")
  print $ evalFailCont $ divInts "13" "42"         -- Right 0
  print $ evalFailCont $ divInts "42" "13"         -- Right 3
  print $ evalFailCont $ divInts "13" "0"          -- Left DivisionByZero
  print $ evalFailCont $ divInts "13" "000"        -- Left DivisionByZero
  print $ evalFailCont $ divInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ findGCD "1" "one"         -- Left (ParseFailed "one")
  print $ evalFailCont $ findGCD "" ""             -- Left EmptyInput
  print $ evalFailCont $ findGCD "100" "64"        -- Right 4
  print $ evalFailCont $ findGCD "5" "7"           -- Right 1
  print $ evalFailCont $ findGCD "0" "7"           -- Right 7 
  print $ evalFailCont $ factorialMult "4" "5"     -- Right 2880
  print $ evalFailCont $ factorialMult "4" "-1"    -- Left (ParseFailed "Factorial of a negative number is undefined")
  print $ evalFailCont $ factorialMult "1" "4"     -- Right 24
