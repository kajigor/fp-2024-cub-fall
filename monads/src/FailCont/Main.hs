module FailCont.Main where

import FailCont.FailCont 
import Text.Read (readMaybe)

data Error 
  = EmptyInput 
  | ParseFailed String 
  | DivisionByZero
  deriving (Show, Eq)

addInts :: String -> String -> FailCont r Error Int
addInts x y = 
    if null x || null y 
        then toFailCont (Left EmptyInput)
        else case (readMaybe x, readMaybe y) of
            (Just a, Just b) -> toFailCont (Right (a + b))
            (Nothing, _)     -> toFailCont (Left (ParseFailed x))
            (_, Nothing)     -> toFailCont (Left (ParseFailed y))

divInts :: String -> String -> FailCont r Error Int 
divInts x y = 
    if null x || null y 
        then toFailCont (Left EmptyInput)
        else case (readMaybe x, readMaybe y) of
            (Just _, Just 0) -> toFailCont (Left DivisionByZero)
            (Just a, Just b) -> toFailCont (Right (a `div` b))
            (Nothing, _)     -> toFailCont (Left (ParseFailed x))
            (_, Nothing)     -> toFailCont (Left (ParseFailed y))

-- average of a list of ints given as strings
averageInts :: [String] -> FailCont r Error Double
averageInts xs = 
    if null xs 
        then toFailCont (Left EmptyInput)
        else let parsed = map readMaybe xs
                 validNumbers = [n | Just n <- parsed]
                 -- get invalid inputs here
                 invalidInputs = [x | (Nothing, x) <- zip parsed xs]
             in if not (null invalidInputs) 
                then toFailCont (Left (ParseFailed (unwords invalidInputs)))
                else toFailCont (Right (sum validNumbers / fromIntegral (length validNumbers)))

main :: IO ()
main = do 
  print $ evalFailCont $ addInts "13" "42"         -- Right 55
  print $ evalFailCont $ addInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ addInts "13" "fourty two" -- Left (ParseFailed "fourty two")
  print $ evalFailCont $ divInts "13" "42"         -- Right 0
  print $ evalFailCont $ divInts "42" "13"         -- Right 3
  print $ evalFailCont $ divInts "13" "0"          -- Left DivisionByZero
  print $ evalFailCont $ divInts "13" "000"        -- Left DivisionByZero
  print $ evalFailCont $ divInts "" "42"           -- Left EmptyInput
  print $ evalFailCont $ averageInts ["1", "2", "3"]         -- Right 2.0
  print $ evalFailCont $ averageInts ["4", "5", "six"]       -- Left (ParseFailed "six")
  print $ evalFailCont $ averageInts []                       -- Left EmptyInput
  print $ evalFailCont $ averageInts ["zero", "1", "2"]      -- Left (ParseFailed "zero")
  print $ evalFailCont $ averageInts ["0", "0", "0"]         -- Right 0.0
  print $ evalFailCont $ averageInts ["10", "20", "30"]      -- Right 20.0