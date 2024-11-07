module Reader.Eval where

import Reader.MyReader
import Expr
import qualified Data.Map as M
import Control.Monad (liftM2)

type VarMap a = M.Map a Int

-- let x = 13 in
-- let y = x + x in
-- y + 1

-- -- Explicit handling of the state 
-- eval :: (Ord a) => Expr a -> (VarMap a -> Int)
-- eval (Num x) _ = x
-- eval (Plus x y) env = eval x env + eval y env
-- eval (Var v) env = 
--   case M.lookup v env of 
--     Just x -> x 
--     Nothing -> error "Oops"
-- eval (Let x v b) env = 
--   let v' = eval v env in 
--   let env' = M.insert x v' env in 
--   eval b env 

-- -- Explicit construction of MyReaders 
-- eval :: Ord a => Expr a -> MyReader (VarMap a) Int
-- eval (Num x) = MyReader $ \_ -> x 
-- eval (Plus x y) = MyReader $ \env ->
--   let xV = runMyReader (eval x) env in
--   let yV = runMyReader (eval y) env in
--   xV + yV
-- eval (Var v) = MyReader $ \env ->
--   case M.lookup v env of
--     Just x -> x
--     Nothing -> error "Oops"
-- eval (Let x v b) = MyReader $ \env ->
--   let vV = runMyReader (eval v) env in
--   let env' = M.insert x vV env in
--   runMyReader (eval b) env'

-- -- Explicit use of binds 
-- eval :: Ord a => Expr a -> MyReader (VarMap a) Int
-- eval (Num x) = pure x
-- eval (Plus x y) =
--   eval x >>= \xV ->
--   eval y >>= \yV ->
--   pure (xV + yV)
-- eval (Var v) =
--   ask >>= \env -> 
--   case M.lookup v env of
--     Just x -> pure x
--     Nothing -> error "Oops"
-- eval (Let x v b) =
--   eval v >>= \vV ->
--   local (\env -> M.insert x vV env) (eval b)

-- -- Do-notation 
-- eval :: Ord a => Expr a -> MyReader (VarMap a) Int
-- eval (Num x) = pure x
-- eval (Plus x y) = do
--   xV <- eval x
--   yV <- eval y
--   pure (xV + yV)
-- eval (Var v) = do
--   env <- ask
--   case M.lookup v env of
--     Just x -> pure x
--     Nothing -> error "Oops"
-- eval (Let x v b) = do
--   vV <- eval v
--   local (M.insert x vV) (eval b)

-- Do-notation, fmaps and app. 
eval :: (Ord a) => Expr a -> MyReader (VarMap a) Int
eval (Num x) = pure x
eval (Plus x y) =
  (+) <$> eval x <*> eval y
eval (Var v) = do
  value <- M.lookup v <$> ask
  case value of
    Just x -> pure x
    Nothing -> error "Oops"
eval (Let x v b) = do
  vV <- eval v
  local (M.insert x vV) (eval b)

-- Getting rid of the error 
evalSafe :: (Ord a) => Expr a -> MyReader (VarMap a) (Maybe Int)
evalSafe (Num x) = pure $ pure x
evalSafe (Plus x y) =
  liftM2 (liftM2 (+)) (evalSafe x) (evalSafe y)
evalSafe (Var v) = do
  M.lookup v <$> ask
evalSafe (Let x v b) = do
  vV <- evalSafe v
  case vV of
    Just vvv -> local (M.insert x vvv) (evalSafe b)
    Nothing -> pure Nothing

runEvalSafe :: (Ord a) => Expr a -> Maybe Int
runEvalSafe e = runMyReader (evalSafe e) M.empty

runEval :: (Ord a) => Expr a -> Int 
runEval e = runMyReader (eval e) M.empty 