module FailCont.Main where

import FailCont.FailCont 
import Text.Read (readMaybe)

data Error 
  = EmptyInput 
  | ParseFailed String 
  | DivisionByZero
  | NotPrime Int
  deriving (Show, Eq)

addInts :: String -> String -> FailCont r Error Int
addInts "" _ = FailCont $ \f -> f (Left EmptyInput)
addInts _ "" = FailCont $ \f -> f (Left EmptyInput)
addInts s1 s2 = FailCont $ \f ->
  case readMaybe s1 of
    Nothing -> f (Left (ParseFailed s1))
    Just x -> case readMaybe s2 of
      Nothing -> f (Left (ParseFailed s2))
      Just y -> f (Right (x + y))

divInts :: String -> String -> FailCont r Error Int 
divInts "" _ = FailCont $ \f -> f (Left EmptyInput)
divInts _ "" = FailCont $ \f -> f (Left EmptyInput)
divInts s1 s2 = FailCont $ \f ->
  case readMaybe s2 of
    Nothing -> f (Left (ParseFailed s2))
    Just 0 -> f (Left DivisionByZero)
    Just x -> case readMaybe s1 of
      Nothing -> f (Left (ParseFailed s1))
      Just 0 -> f (Left DivisionByZero)
      Just y -> f (Right (y `div` x))

notPrime :: Int -> Bool
notPrime 1 = True
notPrime 2 = False
notPrime n = if n `mod` 2 == 0 then True else (any (\x -> n `mod` x == 0) [3,5..(n-2)])

getLegendreSymbol :: Int -> Int -> Int
getLegendreSymbol a p = if a `mod` p == 0 then 0 else if (a ^ ((p - 1) `div` 2)) `mod` p == 1 then 1 else -1

legendreSymbol :: String -> String -> FailCont r Error Int
legendreSymbol "" _ = FailCont $ \f -> f (Left EmptyInput)
legendreSymbol _ "" = FailCont $ \f -> f (Left EmptyInput)
legendreSymbol s1 s2 = FailCont $ \f -> 
  case readMaybe s2 of
    Nothing -> f (Left (ParseFailed s2))
    Just p -> if notPrime p then f (Left (NotPrime p)) else 
      case readMaybe s1 of
        Nothing -> f (Left (ParseFailed s1))
        Just a -> f (Right (getLegendreSymbol a p))

main = do 
  print $ evalFailCont $ addInts "13" "42"         -- Right 55
  print $ evalFailCont $ addInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ addInts "13" "fourty two" -- Left (ParseFailed "fourty two")
  print $ evalFailCont $ divInts "13" "42"         -- Right 0
  print $ evalFailCont $ divInts "42" "13"         -- Right 3
  print $ evalFailCont $ divInts "13" "0"          -- Left DivisionByZero
  print $ evalFailCont $ divInts "13" "000"        -- Left DivisionByZero
  print $ evalFailCont $ divInts "" "42"           -- Left EmptyInput

  print $ evalFailCont $ legendreSymbol "" "13"    -- Left EmptyInput
  print $ evalFailCont $ legendreSymbol "1" ""     -- Left EmptyInput
  print $ evalFailCont $ legendreSymbol "1" "x"    -- Left (ParseFailed "x")
  print $ evalFailCont $ legendreSymbol "x" "1"    -- Left (NotPrime 1)
  print $ evalFailCont $ legendreSymbol "x" "2"    -- Left (ParseFailed "x")
  print $ evalFailCont $ legendreSymbol "1" "4"    -- Left (NotPrime 4)
  print $ evalFailCont $ legendreSymbol "1" "13"   -- Right 1
  print $ evalFailCont $ legendreSymbol "2" "13"   -- Right (-1)
  print $ evalFailCont $ legendreSymbol "6" "3"    -- Right 0
