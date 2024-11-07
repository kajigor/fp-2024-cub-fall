module FailCont.FailCont where 

-- r is the result 
-- e is the type of a failure
-- a is the type of a success
newtype FailCont r e a = FailCont { runFailCont :: (a -> r) -> (e -> r) -> r }

toFailCont :: Either e a -> FailCont r e a
toFailCont (Left e) = FailCont $ \_ f -> f e
toFailCont (Right a) = FailCont $ \s _ -> s a

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont (FailCont m) = m Right Left

instance Functor (FailCont r e) where
    fmap f (FailCont m) = FailCont $ \s e -> m (s . f) e

instance Applicative (FailCont r e) where
    pure a = FailCont $ \s _ -> s a
    (<*>) (FailCont mf) (FailCont ma) = FailCont $ \s e ->
        mf (\f -> ma (s . f) e) e

instance Monad (FailCont r e) where
    return = pure
    (>>=) (FailCont m) f = FailCont $ \s e ->
        m (\a -> runFailCont (f a) s e) e
