module FailCont.FailCont where 

-- r is the result 
-- e is the type of a failure
-- a is the type of a success
newtype FailCont r e a = FailCont ((a -> r) -> (e -> r) -> r)

toFailCont :: Either e a -> FailCont r e a
toFailCont (Right a) = FailCont $ \OnSuccess _ -> OnSuccess a
toFailCont (Left e) = FailCont $ \_ OnFailure -> OnFailure e

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont (FailCont f) = f Right Left

instance Functor (FailCont r e) where
  fmap f (FailCont cont) = FailCont $ \OnSuccess OnFailure ->
        cont (OnSuccess . f) OnFailure


instance Applicative (FailCont r e) where
  pure x = FailCont $ \OnSuccess _ ->OnSuccess x
  (FailCont f) <*> (FailCont x) = FailCont $ \OnSuccess OnFailure ->
    f (\f' -> x (onSuccess . f') onFailure) onFailure


instance Monad (FailCont r e) where
  return = pure
    (FailCont x) >>= f = FailCont $ \onSuccess onFailure ->
      x (\a -> let (FailCont y) = f a in y OnSuccess onFailure) OnFailure
