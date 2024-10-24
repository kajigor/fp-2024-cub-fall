module HW.Eval where

import qualified Data.Map as M
import State.MyState
import HW.StackMachine

-- Possible errors during evaluation
data Error v
  = StackUnderflow (StackInstr v) -- Not enough numbers on the stack to process the instruction
  | VarUndefined v           -- The variable is not defined in the environment 
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
execInstr (PushNum value) = do 
  modify (\s -> s {getStack = value : getStack s})
  return $ Right ()

execInstr (PushVar name) = do
  map <- gets getEnv
  case M.lookup name map of
    Just x -> do
      modify (\s -> s {getStack = x : getStack s})
      return $ Right ()
    Nothing ->   
      return $ Left $ VarUndefined name

execInstr Add = do
  stack <- gets getStack
  case stack of
    (x: y : xs) -> do
      modify (\s -> s {getStack = (x + y):xs})
      return $ Right ()
    _ -> return $ Left $ StackUnderflow Add

execInstr (StoreVar v) = do
  MachineState stack env <- get
  case stack of
    (x : xs) -> do
      modify (\s -> s {getStack = xs, getEnv = M.insert v x env})
      return $ Right ()
    _ -> return $ Left (StackUnderflow (StoreVar v))


-- Execute a list of instructions starting from the given state. 
execProgram :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)
execProgram [] state = 
  case getStack state of
    [result] -> Right state
    stack    -> Left (StackNotExhausted stack) 

execProgram (instr:instrs) state =
  case runMyState (execInstr instr) state of
    (_, Left err) -> Left err 
    (newState, Right()) -> execProgram instrs newState 