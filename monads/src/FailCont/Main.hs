module FailCont.Main where

import FailCont.FailCont 
import Text.Read (readMaybe)
import Data.Numbers.Primes (isPrime)

data Error 
  = EmptyInput 
  | ParseFailed String 
  | DivisionByZero
  deriving (Show, Eq)

readInt :: String -> Either Error Int
readInt "" = Left EmptyInput
readInt s = maybe (Left $ ParseFailed s) Right (readMaybe s)

intBinop :: (Int -> Int -> Either Error a) -> String -> String -> FailCont r Error a
intBinop op s1 s2 = toFailCont $ do
  a <- readInt s1
  b <- readInt s2
  op a b

addInts :: String -> String -> FailCont r Error Int
addInts = intBinop (\a b -> Right (a + b))

divInts :: String -> String -> FailCont r Error Int 
divInts = intBinop safeDiv where
  safeDiv _ 0 = Left DivisionByZero
  safeDiv a b = Right $ a `div` b

checkGaussianPrime :: String -> String -> FailCont r Error Bool
checkGaussianPrime = intBinop $ \a b -> Right $ isGaussianPrime a b where
  isGaussianPrime a 0 = isPrime a && a `mod` 4 == 3
  isGaussianPrime 0 b = isPrime b && b `mod` 4 == 3
  isGaussianPrime a b = isPrime $ a*a + b*b

main = do 
  print $ evalFailCont $ addInts "13" "42"              -- Right 55
  print $ evalFailCont $ addInts "" "42"                -- Left EmptyInput
  print $ evalFailCont $ addInts "13" "fourty two"      -- Left (ParseFailed "fourty two")
  print $ evalFailCont $ divInts "13" "42"              -- Right 0
  print $ evalFailCont $ divInts "42" "13"              -- Right 3
  print $ evalFailCont $ divInts "13" "0"               -- Left DivisionByZero
  print $ evalFailCont $ divInts "13" "000"             -- Left DivisionByZero
  print $ evalFailCont $ divInts "" "42"                -- Left EmptyInput
  print $ evalFailCont $ checkGaussianPrime "13" "42"   -- Right True
  print $ evalFailCont $ checkGaussianPrime "" "42"     -- Left EmptyInput
  print $ evalFailCont $ checkGaussianPrime "0" "0"     -- Right False
  print $ evalFailCont $ checkGaussianPrime "0" "3"     -- Right True
