module FailCont.Main where

import FailCont.FailCont 
import Text.Read (readMaybe)

data Error 
  = EmptyInput 
  | ParseFailed String 
  | DivisionByZero
  deriving (Show, Eq)

parseInput :: String -> FailCont r Error Int
parseInput "" = toFailCont $ Left EmptyInput
parseInput s  = toFailCont $ maybe (Left (ParseFailed s)) Right (readMaybe s)

addInts :: String -> String -> FailCont r Error Int
addInts x y = do
  a <- parseInput x
  b <- parseInput y
  return (a + b)

divInts :: String -> String -> FailCont r Error Int
divInts x y = do
  a <- parseInput x
  b <- parseInput y
  toFailCont $ if b == 0
               then Left DivisionByZero
               else Right (a `div` b)

sumRange :: String -> String -> FailCont r Error Int
sumRange x y = do
  a <- parseInput x
  b <- parseInput y
  toFailCont $ if a > b
               then Left (ParseFailed "a must be less than or equal to b")
               else Right (sum [a..b])

main = do 
  print $ evalFailCont $ addInts "13" "42"         -- Right 55
  print $ evalFailCont $ addInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ addInts "13" "fourty two" -- Left (ParseFailed "fourty two")
  print $ evalFailCont $ divInts "13" "42"         -- Right 0
  print $ evalFailCont $ divInts "42" "13"         -- Right 3
  print $ evalFailCont $ divInts "13" "0"          -- Left DivisionByZero
  print $ evalFailCont $ divInts "13" "000"        -- Left DivisionByZero
  print $ evalFailCont $ divInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ sumRange "1" "5"          -- Right 15 {1 + 2 + 3 + 4 + 5}
  print $ evalFailCont $ sumRange "5" "1"          -- Left (ParseFailed "a must be less than or equal to b")
  print $ evalFailCont $ sumRange "10" "10"        -- Right 10
  print $ evalFailCont $ sumRange "" "5"           -- Left EmptyInput
  print $ evalFailCont $ sumRange "a" "5"          -- Left (ParseFailed "a")
  print $ evalFailCont $ sumRange "1" "b"          -- Left (ParseFailed "b")
