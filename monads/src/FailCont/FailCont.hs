module FailCont.FailCont where

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