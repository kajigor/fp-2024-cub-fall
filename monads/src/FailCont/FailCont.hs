module FailCont.FailCont where

-- r is the result type
-- e is the type of a failure
-- a is the type of a success
newtype FailCont r e a = FailCont { runFailCont :: (a -> r) -> (e -> r) -> r }

toFailCont :: Either e a -> FailCont r e a
toFailCont (Left e)  = FailCont $ \_ fail -> fail e
toFailCont (Right a) = FailCont $ \success _ -> success a

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont fc = runFailCont fc Right Left

instance Functor (FailCont r e) where
  fmap f fc = FailCont $ \success fail -> runFailCont fc (success . f) fail

instance Applicative (FailCont r e) where
  pure x = FailCont $ \success _ -> success x
  ff <*> fa = FailCont $ \success fail ->
    runFailCont ff (\f -> runFailCont fa (success . f) fail) fail

instance Monad (FailCont r e) where
  return = pure
  fa >>= f = FailCont $ \success fail ->
    runFailCont fa (\a -> runFailCont (f a) success fail) fail
