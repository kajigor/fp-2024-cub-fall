module FailCont.FailCont where

-- r is the result
-- e is the type of a failure
-- a is the type of a success
newtype FailCont r e a = FailCont{ runFailCont ::( Either e a -> r) -> r}

toFailCont :: Either e a -> FailCont r e a
toFailCont x = FailCont $ \f -> f x

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont (FailCont cont) = cont id

instance Functor (FailCont r e) where
    fmap f (FailCont cont) = FailCont $ \k ->
        cont (\result -> k (fmap f result))

instance Applicative (FailCont r e) where
    pure x = FailCont $ \k -> k (Right x)
    FailCont f <*> FailCont x = FailCont $ \k ->
        f (\fResult -> x (\xResult -> k (fResult <*> xResult)))

instance Monad (FailCont r e) where
    FailCont m >>= f = FailCont $ \k ->
        m (\result -> case result of
            Left err -> k (Left err)
            Right val -> runFailCont (f val) k)
