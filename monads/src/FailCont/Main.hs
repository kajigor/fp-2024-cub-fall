module FailCont.Main where

import FailCont.FailCont 
import Text.Read (readMaybe)

data Error 
  = EmptyInput 
  | ParseFailed String 
  | DivisionByZero
  deriving (Show, Eq)

parseInt "" = Left EmptyInput
parseInt str = case readMaybe str of
    Nothing -> Left (ParseFailed str)
    Just n  -> Right n

addInts :: String -> String -> FailCont r Error Int
addInts xStr yStr = toFailCont $ do
    x <- parseInt xStr
    y <- parseInt yStr
    Right (x + y)

divInts :: String -> String -> FailCont r Error Int 
divInts xStr yStr = toFailCont $ do
    x <- parseInt xStr
    y <- parseInt yStr
    if y == 0
        then Left DivisionByZero
        else Right (x `div` y)

calculateExpression :: String -> FailCont r Error Int
calculateExpression expr = toFailCont $ do
    let tokens = words expr
    case tokens of
        [xStr, "+", yStr] -> Right <$> evalOp addInts xStr yStr
        [xStr, "-", yStr] -> Right <$> evalOp subtractInts xStr yStr
        [xStr, "/", yStr] -> Right <$> evalOp divInts xStr yStr
        [xStr, "*", yStr] -> Right <$> evalOp multiplyInts xStr yStr
        _                 -> Left (ParseFailed "Invalid expression")

subtractInts :: String -> String -> FailCont r Error Int
subtractInts xStr yStr = toFailCont $ do
    x <- parseInt xStr
    y <- parseInt yStr
    Right (x - y)

multiplyInts :: String -> String -> FailCont r Error Int
multiplyInts xStr yStr = toFailCont $ do
    x <- parseInt xStr
    y <- parseInt yStr
    Right (x * y)

evalOp :: (String -> String -> FailCont r Error Int) -> String -> String -> FailCont r Error Int
evalOp operation xStr yStr = operation xStr yStr

main = do 
  print $ evalFailCont $ addInts "13" "42"         -- Right 55
  print $ evalFailCont $ addInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ addInts "13" "fourty two" -- Left (ParseFailed "fourty two")
  print $ evalFailCont $ divInts "13" "42"         -- Right 0
  print $ evalFailCont $ divInts "42" "13"         -- Right 3
  print $ evalFailCont $ divInts "13" "0"          -- Left DivisionByZero
  print $ evalFailCont $ divInts "13" "000"        -- Left DivisionByZero
  print $ evalFailCont $ divInts "" "42"           -- Left EmptyInput