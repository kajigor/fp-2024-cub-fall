module FailCont.Main where

import FailCont.FailCont
import Text.Read (readMaybe)

data Error
  = EmptyInput
  | ParseFailed String
  | DivisionByZero
  | NegativeInput
  deriving (Show, Eq)

addInts :: String -> String -> FailCont r Error Int
addInts x y =
    case (readMaybe x :: Maybe Int, readMaybe y :: Maybe Int) of
        (Nothing, _) -> toFailCont (Left EmptyInput)
        (_, Nothing) -> toFailCont (Left (ParseFailed y))
        (Just a, Just b) -> toFailCont (Right (a + b))

divInts :: String -> String -> FailCont r Error Int
divInts x y =
    case (readMaybe x :: Maybe Int, readMaybe y :: Maybe Int) of
        (Nothing, _) -> toFailCont (Left EmptyInput)
        (_, Nothing) -> toFailCont (Left (ParseFailed y))
        (Just a, Just 0) -> toFailCont (Left DivisionByZero)
        (Just a, Just b) -> toFailCont (Right (a `div` b))

factorial :: String -> FailCont r Error Int
factorial input =
    case readMaybe input :: Maybe Int of
        Nothing -> toFailCont (Left $ ParseFailed input)
        Just n
            | n < 0     -> toFailCont (Left (NegativeInput))
            | otherwise -> toFailCont (Right (fact n))
    where
    fact 0 = 1
    fact n = n * fact (n - 1)

main = do
  print $ evalFailCont $ addInts "13" "42"         -- Right 55
  print $ evalFailCont $ addInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ addInts "13" "fourty two" -- Left (ParseFailed "fourty two")
  print $ evalFailCont $ divInts "13" "42"         -- Right 0
  print $ evalFailCont $ divInts "42" "13"         -- Right 3
  print $ evalFailCont $ divInts "13" "0"          -- Left DivisionByZero
  print $ evalFailCont $ divInts "13" "000"        -- Left DivisionByZero
  print $ evalFailCont $ divInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ factorial "10"            -- Right 3628800
  print $ evalFailCont $ factorial "aaaaa"         -- Left (ParseFailed "aaaaa")
  print $ evalFailCont $ factorial "-3"           -- Left NegativeInput
  print $ evalFailCont $ factorial ""             -- Left (ParseFailed "")
