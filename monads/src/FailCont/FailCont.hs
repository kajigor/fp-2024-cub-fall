module FailCont.FailCont where 

-- r is the result 
-- e is the type of a failure
-- a is the type of a success
newtype FailCont r e a = FailCont { runFailCont :: (a -> r) -> (e -> r) -> r }

toFailCont :: Either e a -> FailCont r e a
toFailCont (Left e) = FailCont $ \_ failure -> failure e
toFailCont (Right a) = FailCont $ \success _ -> success a

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont (FailCont run) = run Right Left

instance Functor (FailCont r e) where
  fmap f (FailCont run) = FailCont $ \success failure ->
    run (success . f) failure

instance Applicative (FailCont r e) where
  pure x = FailCont $ \success _ -> success x

  (FailCont f) <*> (FailCont x) = FailCont $ \success failure ->
    f (\g -> x (success . g) failure) failure

instance Monad (FailCont r e) where
  (FailCont run) >>= f = FailCont $ \success failure ->
    run (\a -> runFailCont (f a) success failure) failure
