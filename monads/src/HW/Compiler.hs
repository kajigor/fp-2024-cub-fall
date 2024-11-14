module HW.Compiler where

import Expr 
import HW.StackMachine
    ( StackInstr(StoreVar, PushNum, PushVar, Add), StackProgram )

compile :: Expr v -> StackProgram v
compile expr = case expr of
    Num n -> [PushNum n]
    Var v -> [PushVar v]
    Plus e1 e2 -> compile e1 ++ compile e2 ++ [Add]
    Let v e1 e2 -> compile e1 ++ [StoreVar v] ++ compile e2