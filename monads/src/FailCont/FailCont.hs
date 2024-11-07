module FailCont.FailCont where 

-- r is the result 
-- e is the type of a failure
-- a is the type of a success
newtype FailCont r e a = FailCont { runFailCont :: (a -> r) -> (e -> r) -> r }

toFailCont :: Either e a -> FailCont r e a
toFailCont (Left e) = FailCont $ \_ onFail -> onFail e
toFailCont (Right a) = FailCont $ \onSuccess _ -> onSuccess a

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont fc = runFailCont fc Right Left

instance Functor (FailCont r e) where
    fmap f (FailCont g) = FailCont $ \onSuccess onFail -> g (onSuccess . f) onFail

instance Applicative (FailCont r e) where
    pure x = FailCont $ \onSuccess _ -> onSuccess x
    FailCont f <*> FailCont x = FailCont $ \onSuccess onFail ->
        f (\g -> x (onSuccess . g) onFail) onFail

instance Monad (FailCont r e) where
    return = pure
    FailCont x >>= f = FailCont $ \onSuccess onFail ->
        x (\a -> runFailCont (f a) onSuccess onFail) onFail 
