module FailCont.FailCont where
newtype FailCont r e a = FailCont { runFailCont :: (a -> r) -> (e -> r) -> r }

toFailCont :: Either e a -> FailCont r e a
toFailCont (Left e) = FailCont $ \_ f -> f e
toFailCont (Right a) = FailCont $ \s _ -> s a
evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont m = runFailCont m Right Left

instance Functor (FailCont r e) where
  fmap f (FailCont m) = FailCont $ \s f' -> m (s . f) f'
instance Applicative (FailCont r e) where
  pure a = FailCont $ \s _ -> s a
  (FailCont mf) <*> (FailCont ma) = FailCont $ \s f -> mf (\g -> ma (s . g) f) f
instance Monad (FailCont r e) where
  (FailCont m) >>= f = FailCont $ \s f' -> m (\x -> runFailCont (f x) s f') f'
