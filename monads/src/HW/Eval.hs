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
  -- added Eq for testing
  deriving (Show, Eq)

-- Run the compiled program on an empty stack and environment
initialState :: MachineState String
initialState = MachineState [] M.empty

-- Execute a single instruction. 
-- Successful evaluation does not produce any useful result: only the effect of modifying state matters. 
execInstr :: (Ord v, Show v) => StackInstr v -> MyState (MachineState v) (Either (Error v) ())
execInstr instr = case instr of
    PushNum num -> do
        modify (\s -> s { getStack = num : getStack s })
        return (Right ())

    PushVar var -> do
        env <- gets getEnv
        case M.lookup var env of
            Just value -> do
                modify (\s -> s { getStack = value : getStack s })
                return (Right ())
            -- show var to convert the variable into a string
            Nothing -> return $ Left $ VarUndefined (show var)

    StoreVar var -> do
        stack <- gets getStack
        case stack of
            x : xs -> do
                modify (\s -> s { getStack = xs, getEnv = M.insert var x (getEnv s) })
                return (Right ())
            [] -> return $ Left $ StackUnderflow instr

    Add -> do
        stack <- gets getStack
        case stack of
            a : b : rest -> do
                modify (\s -> s { getStack = (a + b) : rest })
                return (Right ())
            _ -> return $ Left $ StackUnderflow instr

-- Execute a list of instructions starting from the given state. 
execProgram :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)
-- eta reduced it
execProgram = execLoop
  where
    -- A function for executing the program instructions iteratively
    execLoop [] currentState = checkFinalState currentState
    execLoop (instr:rest) currentState =
        let (newState, result) = runMyState (execInstr instr) currentState
         in case result of
              Left err -> Left err
              Right _  -> execLoop rest newState

    -- Check if the stack has a single element at the end
    checkFinalState finalState =
        case getStack finalState of
            [oneElement] -> Right finalState
            _ -> Left (StackNotExhausted (getStack finalState))