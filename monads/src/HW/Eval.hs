module HW.Eval where

import qualified Data.Map as M
import State.MyState
import HW.StackMachine

data Error v
  = StackUnderflow (StackInstr v)
  | VarUndefined String
  | StackNotExhausted Stack
  | FinalStackEmpty
  deriving (Show, Eq)


type Stack = [Int]

type Env v = M.Map v Int

data MachineState v = MachineState
  { getStack :: Stack,
    getEnv :: Env v
  }
  deriving (Show, Eq)

initialState :: MachineState String
initialState = MachineState [] M.empty

execInstr :: (Ord v, Show v) => StackInstr v -> MyState (MachineState v) (Either (Error v) ())
execInstr instr = do
  MachineState stack env <- get
  case instr of
    PushNum n -> do
      put $ MachineState (n : stack) env
      return $ Right ()
    PushVar v ->
      case M.lookup v env of
        Just val -> do
          put $ MachineState (val : stack) env
          return $ Right ()
        Nothing -> return $ Left (VarUndefined (show v))
    Add ->
      case stack of
        x : y : rest -> do
          let result = y + x
          put $ MachineState (result : rest) env
          return $ Right ()
        _ -> return $ Left (StackUnderflow Add)
    StoreVar v ->
      case stack of
        x : rest -> do
          put $ MachineState rest (M.insert v x env)
          return $ Right ()
        _ -> return $ Left (StackUnderflow (StoreVar v))

execProgram :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)
execProgram = go

go :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)
go [] state =
  case getStack state of
    [ _ ] -> Right state
    []    -> Left FinalStackEmpty
    stack -> Left (StackNotExhausted stack)
go (instr : instrs) state =
  let (newState, result) = runMyState (execInstr instr) state
   in case result of
        Left err -> Left err
        Right () -> go instrs newState

