module FailCont.Main where

import FailCont.FailCont 
import Text.Read (readMaybe)

data Error 
  = EmptyInput 
  | ParseFailed String 
  | DivisionByZero
  | NegativeRadius
  deriving (Show, Eq)

liftMaybe :: Error -> Maybe a -> FailCont r Error a
liftMaybe err Nothing  = FailCont $ \c -> c (Left err)
liftMaybe _ (Just x) = pure x

calculateCircleArea :: String -> FailCont r Error Double
calculateCircleArea s = do
  rd <- liftMaybe (ParseFailed s) (readMaybe s)
  if rd < 0 
    then FailCont $ \f -> f $ Left NegativeRadius
    else return $ pi * rd * rd

addInts :: String -> String -> FailCont r Error Int
addInts "" s2 = FailCont $ \f -> f (Left EmptyInput)
addInts s1 "" = FailCont $ \f -> f (Left EmptyInput)
addInts s1 s2 = do
  a <- liftMaybe (ParseFailed s1) (readMaybe s1)
  b <- liftMaybe (ParseFailed s2) (readMaybe s2)
  return (a + b)

divInts :: String -> String -> FailCont r Error Int 
divInts "" s2 = FailCont $ \f -> f (Left EmptyInput)
divInts s1 "" = FailCont $ \f -> f (Left EmptyInput)
divInts s1 s2 = do
  a <- liftMaybe (ParseFailed s1) (readMaybe s1)
  b <- liftMaybe (ParseFailed s2) (readMaybe s2)
  if b == 0 
    then FailCont $ \f -> f (Left DivisionByZero)
    else return (a `div` b)

main = do 
  print $ evalFailCont $ addInts "13" "42"         -- Right 55
  print $ evalFailCont $ addInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ addInts "13" "fourty two" -- Left (ParseFailed "fourty two")
  print $ evalFailCont $ divInts "13" "42"         -- Right 0
  print $ evalFailCont $ divInts "42" "13"         -- Right 3
  print $ evalFailCont $ divInts "13" "0"          -- Left DivisionByZero
  print $ evalFailCont $ divInts "13" "000"        -- Left DivisionByZero
  print $ evalFailCont $ divInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ calculateCircleArea "-1"     -- Left NegativeRadius
  print $ evalFailCont $ calculateCircleArea "1"      -- Right 3.141592653589793
