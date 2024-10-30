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
execInstr (PushNum number) = do
  state <- get
  fmap Right (put state { getStack = number : getStack state })

execInstr (PushVar variable) = do
  state <- get
  case M.lookup variable (getEnv state) of
    Just value -> fmap Right (put state { getStack = value : getStack state })
    Nothing -> return (Left (VarUndefined (show variable)))

execInstr (StoreVar variable) = do
  state <- get
  case getStack state of
    x:xs -> fmap Right (put state { getStack = xs, getEnv = M.insert variable x (getEnv state) })
    _ -> return (Left (StackUnderflow (StoreVar variable)))

execInstr Add = do
  state <- get
  case getStack state of
    x:y:xs -> fmap Right (put state { getStack = (x + y) : xs })
    _ -> return (Left (StackUnderflow Add))

-- Execute a list of instructions starting from the given state. 
execProgram :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)

execProgram [] state =
  case getStack state of
    [_] -> Right state
    _ -> Left (StackNotExhausted (getStack state))

execProgram (instr:rest) state =
  let (newState, result) = runMyState (execInstr instr) state
  in case result of
    Left err -> Left err
    Right _  -> execProgram rest newState
