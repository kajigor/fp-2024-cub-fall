module HW.Eval where

import qualified Data.Map as M
import State.MyState
import HW.StackMachine

-- Possible errors during evaluation
data Error v
  = StackUnderflow (StackInstr v) -- Not enough numbers on the stack to process the instruction
  | VarUndefined String           -- The variable is not defined in the environment 
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
execInstr (PushNum x) = do
  modify $ \state -> 
    state { getStack = x : getStack state }
  return $ Right ()

execInstr (PushVar v) = do
  env <- gets getEnv
  case M.lookup v env of
    Nothing -> return $ Left $ VarUndefined $ show v
    Just val -> do 
      modify $ \state -> 
        state { getStack = val : getStack state }
      return $ Right ()
  
execInstr Add = do
  stack <- gets getStack
  case stack of 
    (f:s:t) -> do
      modify $ \state ->
        state { getStack = (f + s) : t }
      return $ Right()
    _ -> return $ Left $ StackUnderflow Add
    
execInstr (StoreVar v) = do
  stack <- gets getStack
  case stack of 
    (f:t) -> do 
      modify $ \state ->
        state { getStack = t, getEnv = M.insert v f $ getEnv state  }
      return $ Right()
    _ -> return $ Left $ StackUnderflow $ StoreVar v

-- Execute a list of instructions starting from the given state. 
execProgram :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)
execProgram [] ms =
  let stack = getStack ms in
  case stack of 
    [_] -> Right ms
    _ -> Left $ StackNotExhausted stack
execProgram (instr:rest) ms = 
  let (result, newState) = runMyState (execInstr instr) ms in
  case newState of
    Left err -> Left err
    Right () -> execProgram rest result