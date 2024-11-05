{-# LANGUAGE InstanceSigs #-}

module FailCont.FailCont where

-- r is the result
-- e is the type of a failure
-- a is the type of a success
newtype FailCont r e a = FailCont {runFailCont :: (Either e a -> r) -> r}

toFailCont :: Either e a -> FailCont r e a
toFailCont (Right a) = FailCont (\r -> r (Right a))
toFailCont (Left e) = FailCont (\r -> r (Left e))

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont f = runFailCont f id

instance Functor (FailCont r e) where
  fmap :: (a -> b) -> FailCont r e a -> FailCont r e b
  fmap f (FailCont x) = FailCont (\m -> x (m . either Left (Right . f)))

instance Applicative (FailCont r e) where
  pure :: a -> FailCont r e a
  pure x = FailCont (\m -> m (Right x))

  (<*>) :: FailCont r e (a -> b) -> FailCont r e a -> FailCont r e b
  -- (((Either e a -> Either e b) -> r)) -> r) -> ((Either e a -> r) -> r) -> ((Either e b -> r ) -> r)
  FailCont f <*> FailCont x =
    FailCont
      ( \m ->
          f
            ( \k -> case k of
                Left e -> m (Left e)
                Right f' -> x (m . fmap f')
            )
      )

instance Monad (FailCont r e) where
  (>>=) :: FailCont r e a -> (a -> FailCont r e b) -> FailCont r e b
  -- ((Either e a -> r) -> r)-> (a -> ((Either e b -> r) -> r)) -> ((Either e b -> r) -> r)
  FailCont x >>= f = FailCont (\m -> x (either (m . Left) (\a -> runFailCont (f a) m)))
