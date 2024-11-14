{-# LANGUAGE InstanceSigs #-}
module Reader.MyReader where 

-- Abstracts consulting some environment while doing computation
newtype MyReader r a = MyReader { runMyReader :: r -> a }

-- Allows access to the environment 
ask :: MyReader r r
ask = MyReader id  

-- Locally modifies the environment 
local :: (r -> r) -> MyReader r a -> MyReader r a 
local f (MyReader g) = MyReader (g . f)

instance Functor (MyReader r) where 
  fmap :: (a -> b) -> MyReader r a -> MyReader r b 
  fmap f (MyReader g) = MyReader (f . g)

instance Applicative (MyReader r) where 
  pure :: a -> MyReader r a 
  pure = MyReader . const

  (<*>) :: MyReader r (a -> b) -> MyReader r a -> MyReader r b
  MyReader f <*> MyReader x = MyReader $ \r ->
    f r (x r)

instance Monad (MyReader r) where 
  (>>=) :: MyReader r a -> (a -> MyReader r b) -> MyReader r b
  MyReader x >>= m = MyReader $ \r -> runMyReader (m (x r)) r 
