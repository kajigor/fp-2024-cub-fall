module FailCont.Main where

import FailCont.FailCont 
import Text.Read (readMaybe)

data Error 
  = EmptyInput 
  | ParseFailed String 
  | DivisionByZero
  deriving (Show, Eq)

withParsedInts :: String -> String -> (Int -> Int -> FailCont r Error Int) -> FailCont r Error Int
withParsedInts a b operation = FailCont $ \failure success ->
  case (a, b) of
    ("", _) -> failure EmptyInput
    (_, "") -> failure EmptyInput
    _ -> case (readMaybe a, readMaybe b) of
      (Just x, Just y) -> runMyFailCont (operation x y) failure success
      (Nothing, _) -> failure (ParseFailed a)
      (_, Nothing) -> failure (ParseFailed b)

addInts :: String -> String -> FailCont r Error Int
addInts a b = withParsedInts a b (\x y -> return (x + y))

divInts :: String -> String -> FailCont r Error Int 
divInts a b = withParsedInts a b $ \x y -> 
  if y == 0 then FailCont $ \failure _ -> failure DivisionByZero
  else return (x `div` y)

avgInts :: String -> String -> FailCont r Error Int
avgInts a b = withParsedInts a b (\x y -> return ((x + y) `div` 2))

diffSquares :: String -> String -> FailCont r Error Int
diffSquares a b = withParsedInts a b $ \x y ->
  if x == y then return 0
  else return (x^2 - y^2)

main = do 
  print $ evalFailCont $ addInts "13" "42"         -- Right 55
  print $ evalFailCont $ addInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ addInts "13" "fourty two" -- Left (ParseFailed "fourty two")
  print $ evalFailCont $ divInts "13" "42"         -- Right 0
  print $ evalFailCont $ divInts "42" "13"         -- Right 3
  print $ evalFailCont $ divInts "13" "0"          -- Left DivisionByZero
  print $ evalFailCont $ divInts "13" "000"        -- Left DivisionByZero
  print $ evalFailCont $ divInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ avgInts "10" "30"         -- Right 20
  print $ evalFailCont $ avgInts "10" ""           -- Left EmptyInput
  print $ evalFailCont $ avgInts "ten" "30"        -- Left (ParseFailed "ten")
  print $ evalFailCont $ diffSquares "5" "3"       -- Right 16
  print $ evalFailCont $ diffSquares "10" "2"      -- Right 96