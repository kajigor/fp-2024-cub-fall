module FailCont.FailCont where 

-- r is the result 
-- e is the type of a failure
-- a is the type of a success
newtype FailCont r e a = FailCont ((a -> r) -> (e -> r) -> r)

toFailCont :: Either e a -> FailCont r e a
toFailCont (Right a) = FailCont $ \onSuccess _ -> onSuccess a
toFailCont (Left e) = FailCont $ \_ onFailure -> onFailure e

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont (FailCont f) = f Right Left

instance Functor (FailCont r e) where
  fmap f (FailCont cont) = FailCont $ \onSuccess onFailure ->
        cont (onSuccess . f) onFailure


instance Applicative (FailCont r e) where
  pure x = FailCont $ \onSuccess _ ->onSuccess x
  (FailCont f) <*> (FailCont x) = FailCont $ \onSuccess onFailure ->
    f (\f' -> x (onSuccess . f') onFailure) onFailure


instance Monad (FailCont r e) where
  return = pure
  (FailCont x) >>= f = FailCont $ \onSuccess onFailure ->
    x (\a -> let (FailCont y) = f a in y onSuccess onFailure)
      onFailure

