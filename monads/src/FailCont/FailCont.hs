{-# LANGUAGE InstanceSigs #-}
module FailCont.FailCont where 

-- r is the result 
-- e is the type of a failure
-- a is the type of a success
newtype FailCont r e a = FailCont { runFailCont :: (e -> r) -> (a -> r) -> r}

toFailCont :: Either e a -> FailCont r e a
toFailCont (Left err) = FailCont $ \f _ -> f err
toFailCont (Right a) = FailCont $ \_ s -> s a

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont (FailCont runFailCont) = runFailCont Left Right

instance Functor (FailCont r e) where
    fmap :: (a -> b) -> FailCont r e a -> FailCont r e b
    -- (a -> b) -> ((e -> r) -> (a -> r) -> r) -> ((e -> r) -> (b -> r) -> r)
    fmap g (FailCont x) = FailCont $ \f s ->
    -- g :: a -> b
    -- x :: (e -> r) -> (a -> r) -> r
    -- s :: b -> r
    -- f :: e -> r
       x f (s . g)

instance Applicative (FailCont r e) where
    pure :: a -> FailCont r e a
    -- pure :: a -> ((e -> r) -> (a -> r) -> r)
    pure x = FailCont $ \f s -> 
    -- x :: a
    -- f :: e -> r
    -- s :: a -> r
       s x

    (<*>) :: FailCont r e (a -> b) -> FailCont r e a -> FailCont r e b
    FailCont g <*> FailCont x = FailCont $ \f s ->
    -- g :: (e -> r) -> ((a -> b) -> r) -> r
    -- x :: (e -> r) -> (a -> r) -> r
    -- s :: b -> r
    -- f :: e -> r
    -- g1 :: (a -> b) -> r) 
       g f $ \g1 -> x f (s . g1)

instance Monad (FailCont r e) where
    (>>=) :: FailCont r e a -> (a -> FailCont r e b) -> FailCont r e b
    FailCont g >>= m = FailCont $ \f s ->
    -- g :: (e -> r) -> (a -> r) -> r
    -- m :: a -> ((e -> r) -> (b -> r) -> r)
    -- f :: e -> r
    -- s :: b -> r
    -- runFailCont (m a) :: (e -> r) -> (b -> r) -> r)
       g f $ \a -> runFailCont (m a) f s
