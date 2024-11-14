module HW.Compiler where

import Expr 
import HW.StackMachine

compile :: Expr v -> StackProgram v
compile (Num n) = [PushNum n]
compile (Var n) = [PushVar n]
compile (Plus e1 e2) = compile e1 ++ compile e2 ++ [Add]
compile (Let v e1 e2) = compile e1 ++ [StoreVar v] ++ compile e2
