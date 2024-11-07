{-# LANGUAGE InstanceSigs #-}
module Cont.MyCont where

newtype MyCont r a = MyCont { runMyCont :: (a -> r) -> r }

evalMyCont :: MyCont r r -> r  
evalMyCont m = runMyCont m id 

instance Functor (MyCont r) where
  fmap :: (a -> b) -> MyCont r a -> MyCont r b
  -- (a -> b) -> ((a -> r) -> r) -> ((b -> r) -> r)
  fmap f (MyCont x) = MyCont $ \k -> 
    -- f :: a -> b 
    -- k ::      b -> r
    -- x :: (a -> r) -> r
    x (k . f) 

instance Applicative (MyCont r) where 
  pure :: a -> MyCont r a 
  -- a -> ((a -> r) -> r)
  pure x = MyCont $ \k -> 
    -- k :: a -> r 
    -- x :: a 
    k x 

  (<*>) :: MyCont r (a -> b) -> MyCont r a -> MyCont r b
  -- (((a -> b) -> r) -> r) -> ((a -> r) -> r) -> ((b -> r) -> r)
  MyCont f <*> MyCont x = MyCont $ \k -> 
    -- f  :: ((a -> b) -> r) -> r
    -- x  :: (a -> r) -> r
    -- k  ::       b -> r 
    -- k1 :: (a -> b) -> r
    -- (.) :: (b -> c) -> (a -> b) -> (a -> c)
    -- b-to-c . a-to-b = \a -> b-to-c (a-to-b a)
    f $ \k1 -> x (k . k1) 

instance Monad (MyCont r) where 
  (>>=) :: MyCont r a -> (a -> MyCont r b) -> MyCont r b 
  -- ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
  MyCont x >>= f = MyCont $ \k -> 
    -- x :: (a -> r) -> r 
    -- f :: a -> ((b -> r) -> r) 
    -- k :: b -> r 
    -- runMyCont (f a) :: (b -> r) -> r
    x $ \a -> runMyCont (f a) k 