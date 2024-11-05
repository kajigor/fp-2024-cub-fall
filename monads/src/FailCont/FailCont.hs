{-# LANGUAGE InstanceSigs #-}
module FailCont.FailCont where 

-- r is the result 
-- e is the type of a failure
-- a is the type of a success
newtype FailCont r e a = FailCont { runFailCont :: (Either e a -> r) -> r }

toFailCont :: Either e a -> FailCont r e a
toFailCont (Right a) = FailCont $ \k -> k (Right a)
toFailCont (Left e)  = FailCont $ \k -> k (Left e)

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont m = runFailCont m id

instance Functor (FailCont r e) where
    fmap :: (a -> b) -> FailCont r e a -> FailCont r e b
    fmap f (FailCont x) = FailCont $ \k -> x (k . fmap f) 
        -- f :: a -> b 
        -- k :: Either e b -> r
        -- x :: (Either e a -> r) -> Either e r

instance Applicative (FailCont r e) where
    pure :: a -> FailCont r e a 
    pure x = FailCont $ \k -> k (Right x)

    (<*>) :: FailCont r e (a -> b) -> FailCont r e a -> FailCont r e b
    FailCont f <*> FailCont x = FailCont $ \k -> 
        -- f      :: ((Either e a -> Either e b) -> r) -> r
        -- x      :: (Either e a -> r) -> r
        -- k      ::  Either e b -> r 
        -- k1     :: (Either e a -> Either e b) -> r
        -- result :: (Either e a -> Either e b) -> r
        -- a      ::  Either e a 
        f $ \result -> case result of
            Left e -> k (Left e) 
            Right g -> x $ \a -> case a of
                Left e -> k (Left e) 
                Right v -> k (Right (g v))  

instance Monad (FailCont r e) where
    return :: a -> FailCont r e a
    return = pure

    (>>=) :: FailCont r e a -> (a -> FailCont r e b) -> FailCont r e b 
    FailCont x >>= f = FailCont $ \k -> 
        -- x      :: (Either e a -> r) -> r 
        -- f      :: a -> ((Either e b -> r) -> r) 
        -- k      :: Either e b -> r 
        -- result :: (Either e a -> r)
        -- runFailCont (f a) :: (Either e b -> r) -> r
        x $ \result -> case result of
            Left e -> k (Left e)  
            Right a -> runFailCont (f a) k  
