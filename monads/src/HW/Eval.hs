module HW.Eval where

import qualified Data.Map as M
import State.MyState
import HW.StackMachine


-- Possible errors during evaluation
data Error v
  = StackUnderflow (StackInstr v) -- Not enough numbers on the stack to process the instruction
  | VarUndefined v                -- The variable is not defined in the environment 
  | StackNotExhausted Stack       -- After the program has finished evaluation, the stack is not a singleton
  deriving (Show, Eq)

type Stack = [Int] -- The stack, holding integers

type Env v = M.Map v Int -- The environment, mapping variables to their values

-- The machine state consists of the stack and the variable environment.
data MachineState v = MachineState
  { getStack :: Stack,
    getEnv :: Env v
  }
  deriving (Show, Eq)

-- Run the compiled program on an empty stack and environment
initialState :: MachineState String
initialState = MachineState [] M.empty

-- Execute a single instruction. 
-- Successful evaluation does not produce any useful result: only the effect of modifying state matters. 
execInstr :: (Ord v, Show v) => StackInstr v -> MyState (MachineState v) (Either (Error v) ())
execInstr (PushNum n) = do
  modify (\(MachineState stack env) -> MachineState (n:stack) env)
  return $ Right ()

execInstr Add = do
  stack <- gets (\(MachineState stack _) -> stack)
  case stack of
    (x:y:xs) -> do
      modify $ \(MachineState _ env) -> MachineState ((x + y):xs) env
      return $ Right ()
    _ -> return $ Left $ StackUnderflow Add

execInstr (StoreVar var) = do
  stack <- gets (\(MachineState stack _) -> stack)
  case stack of
    (x:xs) -> do
      modify $ \(MachineState _ env) -> MachineState xs (M.insert var x env)
      return $ Right ()
    _ -> return $ Left $ StackUnderflow (StoreVar var)

execInstr (PushVar v) = do
  env <- gets (\(MachineState _ env) -> env)
  case M.lookup v env of
    Just x -> do
      modify (\(MachineState stack env) -> MachineState (x:stack) env)
      return $ Right ()
    Nothing -> return $ Left $ VarUndefined v

-- Execute a list of instructions starting from the given state. 
execProgram :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)
execProgram [] state = Right state
execProgram (instr:instrs) state = do
  case runMyState (execInstr instr) state of
    (finalState, Left err) -> Left err
    (finalState, Right ()) -> execProgram instrs finalState




