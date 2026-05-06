{-|
Module      : Complete
Description : Here, we define the 'complete' function which proposes (and ranks) tactics.
-}
module Complete where

import Tactic
import StateHandling
import AST
import Data.List as List
import Data.Map as Map

-- | Variable generator for the 'complete' function:
-- Considering the current synthesis state,
-- finds a new variable name that will not collide with
-- the context.
variableGen :: SynthesisState -> String
variableGen ss =
  let
    varNames = List.map (\n -> "x" ++ show n) [1..]
    unavailable = (keys $ globalCtxt ss) ++ (keys $ localCtxt ss)
  in

  head $ List.filter (\v -> not (elem v unavailable)) varNames



-- | Given the current state and a list of tactics,
-- lists all such tactics from most useful to least.
complete :: SynthesisState -> [Tactic] -> [(Tactic, ReplaceFocus)]
complete state tactics =
  reverse $ sortOn (ranking . fst) (valid tactics)
  where
    valid [] = []
    valid (t : ts) =
      case tryTactic state t of
        Nothing -> valid ts
        Just e -> (t, e) : (valid ts)



-- | Obtains a list of all tactics that could potentially be
-- useful for a given context.
tacticList :: SynthesisState -> [Tactic]
tacticList ss =
  let
    localEnv = keys $ localCtxt ss
    varLocals = List.map varLocalTactic localEnv
    globalEnv = keys $ globalCtxt ss
    varGlobals = List.map (\var -> varGlobalTactic (varGlobalShift var) var) globalEnv
    bools = [boolTactic True, boolTactic False]
    ints = List.map intTactic [0..10]
    intros = introTactic (variableGen ss)
    localApplys = List.map (\func -> localApplyTactic (localApplyShift func) func) localEnv
    globalApplys = List.map (\func -> globalApplyTactic (globalApplyShift func) func) globalEnv
  in
    varLocals ++ varGlobals ++ bools ++ ints ++ localApplys ++ globalApplys ++ [casesTactic, intros, genApplyTactic]
  where
    varGlobalShift var = countTypeVarsInGlobal ss var
    localApplyShift func = countParamsInLocal ss func
    globalApplyShift func = (countParamsInGlobal ss func) + (countTypeVarsInGlobal ss func)