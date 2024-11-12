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
execInstr instr = do
    MachineState stack env <- get
    case instr of
        PushNum a -> do
            put $ MachineState (a : stack) env
            return $ Right ()

        PushVar a -> 
            case M.lookup a env of
                Nothing -> return $ Left (VarUndefined a)
                Just val -> do
                    put $ MachineState (val : stack) env
                    return $ Right ()
        
        Add -> 
            case stack of
                (a : b : xs) -> do
                    let result = a + b
                    put $ MachineState (result : xs) env
                    return $ Right ()
                _ -> return $ Left (StackUnderflow instr)
        
        StoreVar a -> 
            case stack of
                [] -> return $ Left (StackUnderflow instr)
                (x : xs) -> do
                    put $ MachineState xs (M.insert a x env)
                    return $ Right ()

-- Execute a list of instructions starting from the given state. 
execProgram :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)
execProgram program initialState = 
    case runMyState (execInstructions program) initialState of
        (_, Left err) -> Left err
        (finalState, _) ->
            if length (getStack finalState) /= 1
                then Left (StackNotExhausted (getStack finalState))
            else Right finalState
    where
        execInstructions [] = return $ Right ()
        execInstructions (instr:rest) = do
            result <- execInstr instr
            case result of
                Right () -> execInstructions rest
                Left err -> return $ Left err