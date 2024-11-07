module FailCont.Main where

import FailCont.FailCont
import Text.Read (readMaybe)

data Error 
  = EmptyInput 
  | ParseFailed String 
  | DivisionByZero
  deriving (Show, Eq)

addInts :: String -> String -> FailCont r Error Int
addInts x y = case (readMaybe x, readMaybe y) of
  (Nothing, _)       -> FailCont $ \_ fail -> fail EmptyInput
  (_, Nothing)       -> FailCont $ \_ fail -> fail (ParseFailed y)
  (Just a, Just b)   -> FailCont $ \success _ -> success (a + b)

divInts :: String -> String -> FailCont r Error Int
divInts x y = case (readMaybe x, readMaybe y) of
  (Nothing, _)       -> FailCont $ \_ fail -> fail EmptyInput
  (_, Nothing)       -> FailCont $ \_ fail -> fail (ParseFailed y)
  (Just _, Just 0)   -> FailCont $ \_ fail -> fail DivisionByZero
  (Just a, Just b)   -> FailCont $ \success _ -> success (a `div` b)

multiplyIfPositive :: String -> String -> FailCont r Error Int
multiplyIfPositive x y = case (readMaybe x, readMaybe y) of
  (Nothing, _)       -> FailCont $ \_ fail -> fail EmptyInput
  (_, Nothing)       -> FailCont $ \_ fail -> fail (ParseFailed y)
  (Just a, Just b) 
    | a > 0 && b > 0 -> FailCont $ \success _ -> success (a * b)
    | otherwise      -> FailCont $ \_ fail -> fail (ParseFailed "Both numbers must be positive")

main = do 
  print $ evalFailCont $ addInts "13" "42"         -- Right 55
  print $ evalFailCont $ addInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ addInts "13" "fourty two" -- Left (ParseFailed "fourty two")
  print $ evalFailCont $ divInts "13" "42"         -- Right 0
  print $ evalFailCont $ divInts "42" "13"         -- Right 3
  print $ evalFailCont $ divInts "13" "0"          -- Left DivisionByZero
  print $ evalFailCont $ divInts "13" "000"        -- Left DivisionByZero
  print $ evalFailCont $ divInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ multiplyIfPositive "3" "4"  -- Right 12
  print $ evalFailCont $ multiplyIfPositive "3" "-4" -- Left (ParseFailed "Both numbers must be positive")
