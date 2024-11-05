module FailCont.Main where

import FailCont.FailCont 
import Text.Read (readMaybe)

data Error 
  = EmptyInput 
  | ParseFailed String 
  | DivisionByZero
  deriving (Show, Eq)

parseInt :: String -> FailCont r Error Int
parseInt str =
    case str of
        "" -> toFailCont (Left EmptyInput)
        _  -> case readMaybe str of
                 Nothing -> toFailCont (Left (ParseFailed str))
                 Just n  -> toFailCont (Right n)

addInts :: String -> String -> FailCont r Error Int
addInts a b = do
    x <- parseInt a
    y <- parseInt b
    toFailCont (Right (x + y))

divInts :: String -> String -> FailCont r Error Int 
divInts a b = do
    x <- parseInt a
    y <- parseInt b
    if y == 0
        then toFailCont (Left DivisionByZero)
        else toFailCont (Right (x `div` y))

powInts :: String -> String -> FailCont r Error Int 
powInts a b = do
    x <- parseInt a
    y <- parseInt b
    toFailCont (Right (x ^ y))

main = do 
  print $ evalFailCont $ addInts "13" "42"         -- Right 55
  print $ evalFailCont $ addInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ addInts "13" "fourty two" -- Left (ParseFailed "fourty two")
  print $ evalFailCont $ divInts "13" "42"         -- Right 0
  print $ evalFailCont $ divInts "42" "13"         -- Right 3
  print $ evalFailCont $ divInts "13" "0"          -- Left DivisionByZero
  print $ evalFailCont $ divInts "13" "000"        -- Left DivisionByZero
  print $ evalFailCont $ divInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ powInts "2" "3"           -- Right 8
  print $ evalFailCont $ powInts "" "3"            -- Left EmptyInput

