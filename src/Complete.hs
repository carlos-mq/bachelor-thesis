module Complete where

import Tactic
import StateHandling
import AST
import Data.List as List
import Data.Map as Map

-- | Variable generator for the 'complete' function
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


{-
Given the current state and a list of tactics,
lists all such tactics from most useful to least.
-}
complete :: SynthesisState -> [Tactic] -> [(Tactic, ReplaceFocus)]
complete state tactics =
  reverse $ sortOn (ranking . fst) (valid tactics)
  where
    valid [] = []
    valid (t : ts) =
      case tryTactic state t of
        Nothing -> valid ts
        Just e -> (t, e) : (valid ts)

{-
Obtains a list of all tactics that could potentially be
useful for a given context.
-}
tacticList :: SynthesisState -> [Tactic]
tacticList ss =
  let
    localEnv = keys $ localCtxt ss
    varLocals = List.map varLocalTactic localEnv
    globalEnv = keys $ globalCtxt ss
    shift = countTypeVarsInGlobal ss
    varGlobals = List.map (\var -> varGlobalTactic (shift var) var) globalEnv
    intro = introTactic (variableGen ss)
  in
    varLocals ++ varGlobals ++ [casesTactic, intro]
