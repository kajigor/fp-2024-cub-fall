module FailCont.Main where

import FailCont.FailCont 
import Text.Read (readMaybe)

data Error 
  = EmptyInput 
  | ParseFailed String 
  | DivisionByZero
  deriving (Show, Eq)

addInts :: String -> String -> FailCont r Error Int
addInts = undefined 

divInts :: String -> String -> FailCont r Error Int 
divInts = undefined 

main = do 
  print $ evalFailCont $ addInts "13" "42"         -- Right 55
  print $ evalFailCont $ addInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ addInts "13" "fourty two" -- Left (ParseFailed "fourty two")
  print $ evalFailCont $ divInts "13" "42"         -- Right 0
  print $ evalFailCont $ divInts "42" "13"         -- Right 3
  print $ evalFailCont $ divInts "13" "0"          -- Left DivisionByZero
  print $ evalFailCont $ divInts "13" "000"        -- Left DivisionByZero
  print $ evalFailCont $ divInts "" "42"           -- Left EmptyInput