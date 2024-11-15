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
compile (Num x) = [PushNum x]
compile (Var v) = [PushVar v]
compile (Plus x y) = compile x ++ compile y ++ [Add]
compile (Let v e b) = compile e ++ [StoreVar v] ++ compile b