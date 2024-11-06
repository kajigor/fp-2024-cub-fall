module FailCont.FailCont where 
import GHC.Base (DoubleBox)

-- r is the result 
-- e is the type of a failure
-- a is the type of a success
newtype FailCont r e a = FailCont { funcFailCont :: (a -> r) -> (e -> r) -> r }

toFailCont :: Either e a -> FailCont r e a
toFailCont (Left e) = FailCont (\_ failureHandler -> failureHandler e)
toFailCont (Right a) = FailCont (\successHandler _ -> successHandler a)

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont (FailCont g) = g Right Left

instance Functor (FailCont r e) where
    fmap f (FailCont g) = 
        FailCont (\successHandler failureHandler -> 
            g (successHandler . f) failureHandler)

instance Applicative (FailCont r e) where
    pure a = FailCont (\successHandler _ -> successHandler a)

    FailCont gf <*> FailCont gx = 
        FailCont (\successHandler failureHandler -> 
            gf (\f -> gx (successHandler . f) failureHandler) failureHandler)

instance Monad (FailCont r e) where
    FailCont g >>= f = 
        FailCont (\successHandler failureHandler -> 
            g (\a -> funcFailCont (f a) successHandler failureHandler) failureHandler)
