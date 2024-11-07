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
        
addInts :: Int -> Int -> FailCont r String Int
addInts x y
    | x < 0 || y < 0 = FailCont $ \failure _ -> failure "Negative numbers not allowed"
    | otherwise      = pure (x + y)

divInts :: Int -> Int -> FailCont r String Int
divInts _ 0 = FailCont $ \failure _ -> failure "Division by zero"
divInts x y = pure (x `div` y)

safeSubtract :: Int -> Int -> FailCont r String Int
safeSubtract x y
    | result < 0 = FailCont $ \failure _ -> failure "Result is negative"
    | otherwise  = pure result
  where
    result = x - y
    
calculation :: Int -> Int -> Int -> FailCont r String Int
calculation a b c = do
    sumAB <- addInts a b
    divResult <- divInts sumAB c
    safeSubtract divResult 10