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
  deriving (Show)

-- Run the compiled program on an empty stack and environment
initialState :: MachineState String
initialState = MachineState [] M.empty

-- Execute a single instruction. 
-- Successful evaluation does not produce any useful result: only the effect of modifying state matters. 
execInstr :: (Ord v, Show v) => StackInstr v -> MyState (MachineState v) (Either (Error v) ())
execInstr (PushNum n) = do
  modify $ \state -> state { getStack = n : getStack state }
  return (Right ())

execInstr (PushVar var) = do
  state <- get
  case M.lookup var (getEnv state) of
    Just value -> do
      modify $ \s -> s { getStack = value : getStack s }
      return (Right ())
    Nothing -> return (Left (VarUndefined (show var))) 

execInstr Add = do
  state <- get
  case getStack state of
    (x:y:rest) -> do
      modify $ \s -> s { getStack = (x + y) : rest }
      return (Right ())
    _ -> return (Left $ StackUnderflow Add)

execInstr (StoreVar var) = do
  state <- get
  case getStack state of
    (top:rest) -> do
      put $ state { getStack = rest, getEnv = M.insert var top (getEnv state)}
      return (Right ())
    _ -> return (Left $ StackUnderflow (StoreVar var))


-- Execute a list of instructions starting from the given state. 
execProgram :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)
execProgram program initialState =
  let runInstructions [] state = 
        case getStack state of
          [singleResult] -> Right state 
          stack -> Left $ StackNotExhausted stack  
      runInstructions (instr:instrs) state = 
        let result = execInstr instr
            (tempState, execResult) = runMyState result state
        in case execResult of
             Left err -> Left err
             Right () -> runInstructions instrs tempState
  in runInstructions program initialState 
