module FailCont.Main where
import FailCont.FailCont
import FailCont.EmailValidator
import Text.Read (readMaybe)

data Error 
  = EmptyInput 
  | ParseFailed String 
  | DivisionByZero
  deriving (Show, Eq)

addInts :: String -> String -> FailCont r Error Int
addInts s1 s2 = do
  i1 <- toFailCont $ checkParse s1
  i2 <- toFailCont $ checkParse s2
  pure $ i1 + i2

divInts :: String -> String -> FailCont r Error Int 
divInts s1 s2 = do
  i1 <- toFailCont $ checkParse s1
  i2 <- toFailCont $ checkParse s2
  if i2 == 0 then FailCont $ \_ f -> f DivisionByZero
  else pure $ i1 `div` i2

checkParse :: String -> Either Error Int
checkParse "" = Left EmptyInput
checkParse s  = case readMaybe s of
                  Just n  -> Right n
                  Nothing -> Left (ParseFailed s)

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing  = Left e
maybeToEither _ (Just a) = Right a

main = do 
  putStrLn "Testing Integer Operations:"
  print $ evalFailCont $ addInts "13" "42"         
  print $ evalFailCont $ addInts "" "42"          
  print $ evalFailCont $ addInts "13" "fourty two" 
  print $ evalFailCont $ divInts "13" "42"         
  print $ evalFailCont $ divInts "42" "13"         
  print $ evalFailCont $ divInts "13" "0"          
  print $ evalFailCont $ divInts "13" "000"        
  print $ evalFailCont $ divInts "" "42"           
  putStrLn "\nTesting EV:"  
  runExamples