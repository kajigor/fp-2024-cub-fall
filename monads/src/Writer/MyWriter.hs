{-# LANGUAGE InstanceSigs #-}
module Writer.MyWriter where 

newtype MyWriter w a = MyWriter { runMyWriter :: (w, a) }

tell :: w -> MyWriter w () 
tell w = undefined

listen :: MyWriter w a -> MyWriter w (w, a)
listen (MyWriter (w, a)) = undefined

censor :: (w -> w) -> MyWriter w a -> MyWriter w a 
censor f (MyWriter (w, x)) = undefined

instance Functor (MyWriter w) where 
  fmap :: (a -> b) -> MyWriter w a -> MyWriter w b 
  fmap f (MyWriter (w, a)) = undefined
    
instance Monoid w => Applicative (MyWriter w) where 
  pure :: a -> MyWriter w a
  pure x = undefined

  (<*>) :: MyWriter w (a -> b) -> MyWriter w a -> MyWriter w b
  MyWriter (w, f) <*> MyWriter (w', x) = undefined

instance Monoid w => Monad (MyWriter w) where 
  (>>=) :: MyWriter w a -> (a -> MyWriter w b) -> MyWriter w b 
  MyWriter (w, x) >>= f = undefined