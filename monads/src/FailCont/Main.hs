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
  (Nothing, _) -> toFailCont (Left EmptyInput)
  (_, Nothing) -> toFailCont (Left EmptyInput)
  (Just a, Just b) -> toFailCont (Right (a +b))
  (Nothing, Just b) -> toFailCont (Left (ParseFailed x))
  (Just a, Nothing) -> toFailCont (Left (ParseFailed y))


divInts :: String -> String -> FailCont r Error Int 
divInts x y = case (readMaybe x, readMaybe y) of
    (Nothing, _)           -> toFailCont (Left EmptyInput)
    (_, Nothing)           -> toFailCont (Left EmptyInput)
    (Just a, Just b)
      | b == 0             -> toFailCont (Left DivisionByZero)
      | otherwise          -> toFailCont (Right (a `div` b))

main = do 
  print $ evalFailCont $ addInts "13" "42"         -- Right 55
  print $ evalFailCont $ addInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ addInts "13" "fourty two" -- Left (ParseFailed "fourty two")
  print $ evalFailCont $ divInts "13" "42"         -- Right 0
  print $ evalFailCont $ divInts "42" "13"         -- Right 3
  print $ evalFailCont $ divInts "13" "0"          -- Left DivisionByZero
  print $ evalFailCont $ divInts "13" "000"        -- Left DivisionByZero
  print $ evalFailCont $ divInts "" "42"           -- Left EmptyInput