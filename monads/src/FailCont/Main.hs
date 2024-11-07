module FailCont.Main where

import FailCont.FailCont
import Text.Read (readMaybe)

data Error
  = EmptyInput
  | ParseFailed String
  | DivisionByZero
  | InvalidTime
  deriving (Show, Eq)

addInts :: String -> String -> FailCont r Error Int
addInts s1 s2 = do
  x <- parseInt s1
  y <- parseInt s2
  return (x + y)

divInts :: String -> String -> FailCont r Error Int
divInts s1 s2 = do
  x <- parseInt s1
  y <- parseInt s2
  if y == 0
    then FailCont $ \_ onFail -> onFail DivisionByZero
    else return (x `div` y)

-- Determines whether 2 numbers are positioned opposite each other on a clock and returns true or false
-- For example 12 and 6 are opposite each other, but so are 0 and 6, as well as 0 and 18 (since 6 is the same as 18 on a clock) 
clockOpposite :: String -> String -> FailCont r Error Bool
clockOpposite s1 s2 = do
  x <- parseInt s1
  y <- parseInt s2
  if x > 23 || x < 0 || y > 23 || y < 0
    then FailCont $ \_ onFail -> onFail InvalidTime
    else if abs (x - y) == 18 || abs (x - y) == 6
      then return True
      else return False

parseInt :: String -> FailCont r Error Int
parseInt "" = FailCont $ \_ onFail -> onFail EmptyInput
parseInt s = case readMaybe s of
  Just n -> return n
  Nothing -> FailCont $ \_ onFail -> onFail (ParseFailed s)


main = do
  putStrLn "first done"
  print $ evalFailCont $ addInts "13" "42"               -- Right 55
  print $ evalFailCont $ addInts "" "42"                 -- Left EmptyInput
  print $ evalFailCont $ addInts "13" "fourty two"       -- Left (ParseFailed "fourty two")
  print $ evalFailCont $ divInts "13" "42"               -- Right 0
  print $ evalFailCont $ divInts "42" "13"               -- Right 3
  print $ evalFailCont $ divInts "13" "0"                -- Left DivisionByZero
  print $ evalFailCont $ divInts "13" "000"              -- Left DivisionByZero
  print $ evalFailCont $ divInts "" "42"                 -- Left EmptyInput
  print $ evalFailCont $ clockOpposite "42" "13"         -- Left InvalidTime
  print $ evalFailCont $ clockOpposite "fourty" "13"     -- Left (ParseFailed "fourty")
  print $ evalFailCont $ clockOpposite "13" ""           -- Left EmptyInput
  print $ evalFailCont $ clockOpposite "22" "4"          -- Right True
  print $ evalFailCont $ clockOpposite "22" "6"          -- Right False
  print $ evalFailCont $ clockOpposite "22" "16"         -- Right True
  print $ evalFailCont $ clockOpposite "22" "-16"        -- Left InvalidTime
