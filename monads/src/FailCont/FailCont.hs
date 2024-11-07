module FailCont.FailCont where 

-- r is the result 
-- e is the type of a failure
-- a is the type of a success
newtype FailCont r e a = FailCont { runFailCont :: (e -> r) -> (a -> r) -> r }

toFailCont :: Either e a -> FailCont r e a
toFailCont (Left e)  = FailCont $ \failure _ -> failure e
toFailCont (Right a) = FailCont $ \_ success -> success a

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont fc = runFailCont fc Left Right 

instance Functor (FailCont r e) where
    fmap f (FailCont cont) = FailCont $ \failure success -> cont failure (success . f)

instance Applicative (FailCont r e) where
    pure x = FailCont $ \_ success -> success x
    FailCont ff <*> FailCont fa = FailCont $ \failure success ->
        ff failure $ \f -> fa failure (success . f)

instance Monad (FailCont r e) where
    FailCont fa >>= f = FailCont $ \failure success ->
        fa failure (\a -> runFailCont (f a) failure success)
        
data Error
  = EmptyInput
  | ParseFailed String
  | DivisionByZero
  | NegativeInput
  deriving (Show, Eq)

parseInt :: String -> Either Error Int
parseInt "" = Left EmptyInput
parseInt str = case readMaybe str of
    Nothing -> Left (ParseFailed str)
    Just n  -> Right n

addInts :: String -> String -> FailCont r Error Int
addInts xStr yStr = toFailCont $ do
    x <- parseInt xStr
    y <- parseInt yStr
    if x < 0 || y < 0
        then Left NegativeInput
        else Right (x + y)

divInts :: String -> String -> FailCont r Error Int
divInts xStr yStr = toFailCont $ do
    x <- parseInt xStr
    y <- parseInt yStr
    if y == 0
        then Left DivisionByZero
        else Right (x `div` y)

safeSubtract :: Int -> Int -> FailCont r Error Int
safeSubtract x y
    | result < 0 = FailCont $ \failure _ -> failure NegativeInput
    | otherwise  = pure result
  where
    result = x - y

calculation :: Int -> Int -> Int -> FailCont r String Int
calculation a b c = do
    sumAB <- addInts a b
    divResult <- divInts sumAB c
    safeSubtract divResult 10