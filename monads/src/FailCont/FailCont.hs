{-# LANGUAGE InstanceSigs #-}
module FailCont.FailCont where 

-- r is the result 
-- e is the type of a failure
-- a is the type of a success
newtype FailCont r e a = FailCont { runMyFailCont :: (e -> r) -> (a -> r) -> r} 

toFailCont :: Either e a -> FailCont r e a
toFailCont (Left e) = FailCont $ \failure success -> failure e 
toFailCont (Right a) = FailCont $ \failure success -> success a

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont fs = runMyFailCont fs Left Right 

instance Functor (FailCont r e) where
    fmap :: (a -> b) -> FailCont r e a -> FailCont r e b 
    fmap f (FailCont x) = FailCont $ \failure success -> x failure (success.f)
        -- f ::     a -> b
        -- success ::    b -> r
        -- failure :: e -> r
        -- x :: (e -> r) -> (a -> r) -> r

instance Applicative (FailCont r e) where
    pure :: a -> FailCont r e a
    pure x = FailCont $ \failure success -> success x

    (<*>) :: FailCont r e (a -> b) -> FailCont r e a -> FailCont r e b
    (<*>) (FailCont f) (FailCont x) = FailCont $ \failure success -> f failure $ \g -> x failure (success . g)
    -- f : (e -> r) -> ((a -> b) -> r) -> r
    -- x : (e -> r) -> (a -> r) -> r
    -- failure : e -> r
    -- success : b -> r
    -- g: (a -> b)
    -- \g -> x failure (success . g): (a -> b) -> r


instance Monad (FailCont r e) where
    (>>=) :: FailCont r e a -> (a -> FailCont r e b) -> FailCont r e b 
    (FailCont x) >>= f = FailCont $ \failure success -> x failure $ \a -> runMyFailCont (f a) failure success
    -- x : (e -> r) -> (a -> r) -> r
    -- f : a -> ((e -> r) -> (b -> r) -> r)
    -- failure : e -> r
    -- success : b -> r