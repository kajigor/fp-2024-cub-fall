module HW.Compiler where

import Expr 
import HW.StackMachine

-- let x = 13 in
-- let y = 42 in
-- let x = (x + y) in
-- (x + y)
-- should compile into 
-- [PushNum 13,StoreVar "x",PushNum 42,StoreVar "y",PushVar "x",PushVar "y",Add,StoreVar "x",PushVar "x",PushVar "y",Add]

-- Compiler of an expression into machine instructions
compile :: Expr v -> StackProgram v
compile (Num n) = [PushNum n]
compile (Var x) = [PushVar x]
compile (Plus a b) = compile a ++ compile b ++ [Add]
compile (Let x a b) = 
  compile a ++           -- Compile the binding expression
  [StoreVar x] ++        -- Store the value for the current scope
  compile b           -- Compile the body