module MyDebugger (
    debugEval,
    DebugStep(..),
) where

import MyRegex
import Text.Printf (printf)

data MatchState = Matched | NotMatched | Calculating
    deriving (Eq)

instance Show MatchState where
    show Matched = "✓"
    show NotMatched = "✗"
    show Calculating = "..."

data DebugStep = DebugStep {
    currentStr :: String,
    pattern :: MyRegex,
    matchState :: MatchState
} deriving (Eq)

instance Show DebugStep where
    show (DebugStep str pat state) =
        printf "%-15s VS %-20s -> %s"
            (show str)
            (show pat)
            (show state)

debugEval :: MyRegex -> String -> [DebugStep]
debugEval r@(Literal c) [x] =
    let matched = c == x
    in [DebugStep [x] r (if matched then Matched else NotMatched)]
debugEval r@(Literal _) s =
    [DebugStep s r NotMatched]

debugEval r@(CharClassMatch cc) [x] =
    let matched = matchCharClass cc x
    in [DebugStep [x] r (if matched then Matched else NotMatched)]
debugEval r@(CharClassMatch _) s =
    [DebugStep s r NotMatched]

debugEval r@(Disjunction a b) s =
    [DebugStep s r Calculating] ++
    debugEval a s ++
    debugEval b s ++
    [DebugStep s r finalState]
    where
        finalState = if eval r s then Matched else NotMatched

debugEval r@(Star regex) s =
    let splits' = filter (\(prefix, _) -> prefix /= "") $ splits s
        submatches = concat [
            debugEval regex prefix ++
            debugEval (Star regex) suffix
            | (prefix, suffix) <- splits']
    in [DebugStep s r Calculating] ++
        submatches ++
        [DebugStep s r finalState]
    where
        finalState = if eval r s then Matched else NotMatched

debugEval r@(Plus regex) s =
    let splits' = filter (\(prefix, _) -> prefix /= "") $ splits s
        submatches = concat [
            debugEval regex prefix ++
            debugEval (Star regex) suffix
            | (prefix, suffix) <- splits']
    in [DebugStep s r Calculating] ++
        submatches ++
        [DebugStep s r finalState]
    where
        finalState = if eval r s then Matched else NotMatched

debugEval r@(Question regex) s =
    let emptyCase = s == ""
        submatches = if not emptyCase then debugEval regex s else []
    in [DebugStep s r Calculating] ++
        submatches ++
        [DebugStep s r finalState]
    where
        finalState = if eval r s then Matched else NotMatched

debugEval r@(Concat a b) s =
    [DebugStep s r Calculating] ++
    tryFirstMatch splits' ++
    [DebugStep s r finalState]
    where
        splits' = splits s
        tryFirstMatch [] = []
        tryFirstMatch ((prefix, suffix):rest)
            | null prefix = tryFirstMatch rest
            | otherwise =
                let aSteps = debugEval a prefix
                    aMatched = matchState (last aSteps) == Matched
                    bSteps = if aMatched then debugEval b suffix else []
                    bMatched = (not (null bSteps) && (matchState (last bSteps) == Matched))
                in if aMatched && bMatched
                   then aSteps ++ bSteps
                   else aSteps ++ bSteps ++ tryFirstMatch rest
        finalState = if eval r s then Matched else NotMatched
