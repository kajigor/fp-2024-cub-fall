module HW.Eval 
  ( Error(..)
  , Stack
  , Env
  , MachineState(..)
  , initialState
  , execInstr
  , execProgram
  ) where

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
  { getStack :: Stack
  , getEnv :: Env v
  } deriving (Show, Eq)

initialState :: MachineState String
initialState = MachineState [] M.empty

execInstr :: (Ord v, Show v) => StackInstr v -> MyState (MachineState v) (Either (Error v) ())
execInstr instr = do
    state <- get
    case instr of
        PushNum n -> do
            put state { getStack = n : getStack state }
            return $ Right ()

        PushVar v -> case M.lookup v (getEnv state) of
            Nothing -> return $ Left $ VarUndefined (show v)
            Just x -> do
                put state { getStack = x : getStack state }
                return $ Right ()

        Add -> case getStack state of
            (x:y:rest) -> do
                put state { getStack = (x + y) : rest }
                return $ Right ()
            _ -> return $ Left $ StackUnderflow Add

        StoreVar v -> case getStack state of
            (x:rest) -> do
                put state { getStack = rest, getEnv = M.insert v x (getEnv state) }
                return $ Right ()
            _ -> return $ Left $ StackUnderflow (StoreVar v)

execProgram :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)
execProgram [] state@(MachineState [x] env) = Right state
execProgram [] (MachineState [] env) = Left FinalStackEmpty
execProgram [] (MachineState stack env) = Left (StackNotExhausted stack)
execProgram (i:is) state = 
    let (state', possibleErr) = runMyState (execInstr i) state
    in case possibleErr of
        Left err -> Left err
        Right () -> execProgram is state'
