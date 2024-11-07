{-# LANGUAGE InstanceSigs #-}
module FailCont.FailCont where 

-- r is the result 
-- e is the type of a failure
-- a is the type of a success
newtype FailCont r e a = FailCont { runFailCont :: (e -> r) -> (a -> r) -> r }

toFailCont :: Either e a -> FailCont r e a
toFailCont (Left e) = FailCont $ \fail _ -> fail e
toFailCont (Right a) = FailCont $ \_ success -> success a


evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont (FailCont x) = x Left Right
-- x :: (e -> (Either e a)) -> (a -> (Either e a)) -> (Either e a)



instance Functor (FailCont r e) where
    fmap :: (a -> b) -> FailCont r e a -> FailCont r e b
    -- x ::    (e -> r) -> (a -> r) -> r
    -- fail :: (e -> r)
    -- f ::                (a -> b)
    -- success ::               (b -> r)
    
    fmap f (FailCont x) = FailCont $ \fail success  -> x fail (success . f)
    

instance Applicative (FailCont r e) where
    pure :: a -> FailCont r e a
    -- success :: (a -> r)
    pure x = FailCont $ \_ success  -> success x
    (<*>) :: FailCont r e (a -> b) -> FailCont r e a -> FailCont r e b
    -- ff ::   (e -> r) -> ((a -> b -> r)) -> r
    -- x ::    (e -> r) -> (a -> r) -> r
    -- fail :: (e -> r)
    -- f ::                (a -> b -> r)
    -- success ::               (b -> r)

    (FailCont ff) <*> (FailCont x) = FailCont $ \fail success  ->
        ff fail (\f -> x fail (success . f)) 

instance Monad (FailCont r e) where
    (>>=) :: FailCont r e a -> (a -> FailCont r e b) -> FailCont r e b
    -- f :: a -> FailCont r e b
    -- x ::    (e -> r) -> (a -> r) -> r
    -- runFailCont         (f a) :: (e -> r) -> (b -> r) -> r
    -- fail ::                      (e -> r)
    -- success ::                               (b -> r)

    (FailCont x) >>= f = FailCont $ \fail success  ->
        x fail (\a -> runFailCont (f a) fail success ) 



