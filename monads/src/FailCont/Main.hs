module FailCont.Main where

import FailCont.FailCont
import Text.Read (readMaybe)

data Error
  = EmptyInput
  | ParseFailed String
  | DivisionByZero
  | NegSqrt
  deriving (Show, Eq)

parser :: String -> Either Error Int
parser s
  | null s = Left EmptyInput
  | otherwise = maybe (Left (ParseFailed s)) Right (readMaybe s)

addInts :: String -> String -> FailCont r Error Int
addInts s1 s2 =
  toFailCont
    ( do
        a <- parser s1
        b <- parser s2
        Right (a + b)
    )

divInts :: String -> String -> FailCont r Error Int
divInts s1 s2 =
  toFailCont
    ( do
        a <- parser s1
        b <- parser s2
        if b == 0 then Left DivisionByZero else Right (a `div` b)
    )

sqrtInts :: String -> FailCont r Error Double
sqrtInts s =
  toFailCont
    ( do
        n <- parser s
        if n < 0 then Left NegSqrt else Right (sqrt (fromIntegral n))
    )

modInts :: String -> String -> FailCont r Error Int
modInts s1 s2 =
  toFailCont
    ( do
        a <- parser s1
        b <- parser s2
        if b == 0 then Left DivisionByZero else Right (a `mod` b)
    )

main = do
  print $ evalFailCont $ addInts "13" "42" -- Right 55
  print $ evalFailCont $ addInts "" "42" -- Left EmptyInput
  print $ evalFailCont $ addInts "13" "fourty two" -- Left (ParseFailed "fourty two")
  print $ evalFailCont $ divInts "13" "42" -- Right 0
  print $ evalFailCont $ divInts "42" "13" -- Right 3
  print $ evalFailCont $ divInts "13" "0" -- Left DivisionByZero
  print $ evalFailCont $ divInts "13" "000" -- Left DivisionByZero
  print $ evalFailCont $ divInts "" "42" -- Left EmptyInput
  print $ evalFailCont $ sqrtInts "25" -- Right 5.0
  print $ evalFailCont $ sqrtInts "-10" -- Left NegSqrt
  print $ evalFailCont $ sqrtInts "ten" -- Left (ParseFailed "ten")
  print $ evalFailCont $ sqrtInts "" -- Left EmptyInput
  print $ evalFailCont $ modInts "13" "3" -- Right 1
  print $ evalFailCont $ modInts "13" "0" -- Left DivisionByZero
  print $ evalFailCont $ modInts "" "2" -- Left EmptyInput
