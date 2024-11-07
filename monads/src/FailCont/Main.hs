module FailCont.Main where

import FailCont.FailCont 
import Text.Read (readMaybe)

data Error 
  = EmptyInput 
  | ParseFailed String 
  | DivisionByZero
  deriving (Show, Eq)

parse :: String -> FailCont r Error Int
parse str = 
    case str of
      "" -> toFailCont $ Left EmptyInput
      _  -> case readMaybe str of
              Nothing -> toFailCont $ Left $ ParseFailed str 
              Just value -> toFailCont $ Right value

addInts :: String -> String -> FailCont r Error Int
addInts x y = do
    a <- parse x
    b <- parse y
    return $ a + b

divInts :: String -> String -> FailCont r Error Int
divInts x y = do
    a <- parse x
    b <- parse y 
    if b == 0 then toFailCont $ Left DivisionByZero else toFailCont $ Right $ a `div` b

-- Example of usage

data FormError = EmptyName | InvalidAge String deriving (Show, Eq)

validateName :: String -> FailCont r FormError String
validateName "" = toFailCont $ Left EmptyName
validateName name = toFailCont $ Right name

validateAge :: String -> FailCont r FormError Int
validateAge ageStr =
    case readMaybe ageStr of
        Nothing -> toFailCont $ Left (InvalidAge ageStr)
        Just age -> toFailCont $ Right age

validateForm :: String -> String -> FailCont r FormError (String, Int)
validateForm name ageStr = do
    validName <- validateName name
    validAge <- validateAge ageStr
    return (validName, validAge)

main = do 
  print $ evalFailCont $ addInts "13" "42"         -- Right 55
  print $ evalFailCont $ addInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ addInts "13" "fourty two" -- Left (ParseFailed "fourty two")
  print $ evalFailCont $ divInts "13" "42"         -- Right 0
  print $ evalFailCont $ divInts "42" "13"         -- Right 3
  print $ evalFailCont $ divInts "13" "0"          -- Left DivisionByZero
  print $ evalFailCont $ divInts "13" "000"        -- Left DivisionByZero
  print $ evalFailCont $ divInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ validateForm "Alice" "25"         -- Right ("Alice", 25)
  print $ evalFailCont $ validateForm "" "25"              -- Left EmptyName
  print $ evalFailCont $ validateForm "Alice" "notAnAge"   -- Left (InvalidAge "notAnAge")