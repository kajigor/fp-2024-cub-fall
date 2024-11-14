-- S = λfgx . f x (g x)
-- K = λxy . x
-- I = λx . x
-- B = λfgx . f (g x)

-- S K K =  (λfgx . f x (g x)) (λxy . x) (λxy . x) =
-- beta  =  (λgx . (λxy . x) x (g x)) (λxy . x)    =
-- alpha =  (λgx . (λzy . z) x (g x)) (λxy . x)    =
-- beta  =  (λx . (λzy . z) x ((λxy . x) x))       =
-- beta  =  (λx . (λy . x) (λy . x))               =
--       =  λx . x

-- S (K S) K = (λfgx . f x (g x)) ((λxy . x) (λfgx . f x (g x))) (λxy . x) =
-- beta      = (λfgx . f x (g x)) (λy . (λfgx . f x (g x))) (λxy . x)      =      
-- beta      = (λx . (λy . (λfgx . f x (g x))) x ((λxy . x) x))            = 
-- beta      = (λx . (λfgx . f x (g x)) ((λxy . x) x))                     =
-- alpha     = (λx . (λfgz . f z (g z)) ((λxy . x) x))                     =
-- beta      = λx . (λfgz . f z (g z)) (λy . x)                            =
-- beta      = λx . (λgz . (λy . x) z (g z))                               =
-- beta      = λx . (λgz . x (g z))                                        =
--           = λxgz . x (g z)
-- alpha     = λfgx . f (g x)