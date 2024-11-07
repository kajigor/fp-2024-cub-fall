{-# LANGUAGE InstanceSigs #-}
module FailCont.FailCont where 

import Data.Function

-- r is the result 
-- e is the type of a failure
-- a is the type of a success
newtype FailCont r e a = FailCont { runFailCont :: (Either e a -> r) -> r }

toFailCont :: Either e a -> FailCont r e a
toFailCont something = FailCont $ \f -> f something

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont m = runFailCont m id

instance Functor (FailCont r e) where
    fmap :: (a -> b) -> (FailCont r e a) -> (FailCont r e b)
    -- fmap :: (a -> b) -> ((Either e a -> r) -> r) -> (Either e b -> r) -> r
    fmap f (FailCont x) = FailCont $ \k -> x (k . fmap f)
        -- f :: a -> b
        -- k :: Either e b -> r
        -- fmap f :: Either e a -> Either e b
        -- k . fmap f :: Either e a -> r
        -- x (k . fmap f) :: r

instance Applicative (FailCont r e) where
    pure :: a -> FailCont r e a
    pure a = FailCont $ \cont -> cont (Right a)
    (<*>) :: FailCont r e (a -> b) -> FailCont r e a -> FailCont r e b
    -- (<*>) :: ((Either e (a -> b) -> r) -> r) -> ((Either e a -> r) -> r) -> (Either e b -> r) -> r
    (FailCont fH) <*> (FailCont x) = FailCont $ \k ->
        fH $ \k1 -> case k1 of
            Left e -> k $ Left e
            Right f -> x (k . fmap f)
        -- fH :: (Either e (a -> b) -> r) -> r
        -- x :: (Either e a -> r) -> r
        -- k :: Either e b -> r
        -- k1 :: Either e (a -> b)
        -- f :: a -> b
        -- fmap f :: Either e a -> Either e b
        -- k . fmap f = Either e a -> r


instance Monad (FailCont r e) where
    (>>=) :: FailCont r e a -> (a -> FailCont r e b) -> FailCont r e b
    -- (>>=) :: ((Either e a -> r) -> r) -> (a -> (Either e b -> r) -> r) -> (Either e b -> r) -> r
    (FailCont x) >>= f = FailCont $ \k ->
        x $ \k1 -> case k1 of
            Left e -> k $ Left e
            Right a -> runFailCont (f a) k
        -- x :: (Either e a -> r) -> r
        -- f :: a -> (Either e b -> r) -> r
        -- k :: Either e b -> r
        -- k1 :: Either e a
