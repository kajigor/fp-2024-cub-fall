module FailCont.FailCont where 

-- r is the result 
-- e is the type of a failure
-- a is the type of a success
newtype FailCont r e a = FailCont { runFailCont :: (Either e a -> r) -> r }

toFailCont :: Either e a -> FailCont r e a
toFailCont v = FailCont $ \f -> f v 

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont fc = runFailCont fc id 

instance Functor (FailCont r e) where
    -- fmap :: (a -> b) -> FailCont r e a -> FailCont r e b
    fmap f (FailCont fc) = FailCont $ \u -> fc (\v -> u (fmap f v))

instance Applicative (FailCont r e) where
    -- pure :: a -> FailCont r e a
    pure x = FailCont $ \f -> f (Right x)
    
    -- (<*>) :: FailCont r e (a -> b) -> FailCont r e a -> FailCont r e b
    FailCont ff <*> FailCont fu = FailCont $ \v ->
        ff $ \f -> 
        fu $ \u -> 
        v (case f of
            Left e  -> Left e
            Right fn -> fmap fn u)


instance Monad (FailCont r e) where
    -- return :: a -> FailCont r e a
    return = pure

    -- (>>=) :: FailCont r e a -> (a -> FailCont r e b) -> FailCont r e b
    FailCont fu >>= g = FailCont $ \v ->
        fu $ \u ->
            case u of 
                Left e -> v $ Left e
                Right val -> runFailCont (g val) v 
