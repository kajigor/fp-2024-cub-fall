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
  deriving (Show, eq)

-- Run the compiled program on an empty stack and environment
initialState :: MachineState String
initialState = MachineState [] M.empty

-- Execute a single instruction. 
-- Successful evaluation does not produce any useful result: only the effect of modifying state matters. 
execInstr :: (Ord v, Show v) => StackInstr v -> MyState (MachineState v) (Either (Error v) ())
execInstr (PushNum x) = do
  -- Push the number to the stack
  modify $ \s -> s { getStack = x : getStack s }
  return $ Right ()

execInstr (PushVar k) = do
  -- Lookup the variable in the environment
  env <- gets getEnv
  case M.lookup k env of
    Just v -> do
      -- Push the variable value onto the stack
      modify $ \s -> s { getStack = v : getStack s }
      return $ Right ()
    Nothing -> return $ Left (VarUndefined k)

execInstr Add = do
  -- Pop two values from the stack, add them, and push the result back
  stack <- gets getStack
  case stack of
    (a:b:rest) -> do
      modify $ \s -> s { getStack = (a + b) : rest }
      return $ Right ()
    _ -> return $ Left (StackUnderflow Add)

execInstr (StoreVar k) = do
  -- Pop the top value from the stack and store it in the environment
  stack <- gets getStack
  case stack of
    (v:rest) -> do
      modify $ \s -> s { getEnv = M.insert k v (getEnv s), getStack = rest }
      return $ Right ()
    _ -> return $ Left (StackUnderflow (StoreVar k)) 

-- Execute a list of instructions starting from the given state. 
execProgram :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)
execProgram [] state = Right state
execProgram (instr:instrs) state =
  case runState (execInstr instr) state of
    (Left err, _) -> Left err
    (Right (), newState) -> execProgram instrs newState
