{-|
Module      : Tactic
Description : Defines the tactics and implements functions to run them.
-}
module Tactic where

import AST
import StateHandling
import Data.Map as Map
import qualified Data.List as List

type Expression = ReplaceFocus

-- * Data Types and Functions for Tactics

-- | Wrapper for tactics, including information such as name, ranking, etc.
data Tactic = Tactic {
  tacticName :: String, -- The name of the tactic.
  tactic :: SynthesisState -> Maybe (Expression, Substitution), -- The underlying function of the tactic.
  willPropagate  :: Bool, -- Whether the tactic propagates substitutions or not.
  counterShift :: Int, -- How much to increment the unification variable counter upon applying this tactic.
  ranking :: Int -- How useful the tactic is.
}

-- | Tries a tactic and possibly returns the synthesized
-- expression.
tryTactic :: SynthesisState -> Tactic -> Maybe Expression
tryTactic ss tac =
  case tactic tac ss of
    Nothing -> Nothing
    Just (e, _) -> Just e

-- | Obtains the synthesis state resulting from running some
-- tactic on the current one.
runTactic :: Tactic -> SynthesisState -> SynthesisState
runTactic tac ss =
  case tactic tac ss of
    Nothing -> ss
    Just (e, subst) ->
      let
        newSs = 
          if willPropagate tac
            then replace e (propagateSubstitution subst ss)
            else replace e ss
      in
        newSs { freshCounter = freshCounter ss + counterShift tac }

-- * Implemented Tactics

-- | The underlying function for the 'intro' tactic.
intro :: String -> SynthesisState -> Maybe (Expression, Substitution)
intro var ss =
  case (Map.lookup var (globalCtxt ss), Map.lookup var (localCtxt ss)) of
    (Nothing, Nothing) ->
      let
        ix = freshCounter ss
      in
        case holeData ss of
          Just (_, Func t1 t2) -> Just (ToLambda var t1 ix t2, empty)
          _ -> Nothing
    _ -> Nothing

-- | The 'intro' tactic.
introTactic :: String -> Tactic
introTactic var = Tactic {
  tacticName = "intro " ++ var,
  tactic = intro var,
  willPropagate = False,
  counterShift = 1,
  ranking = 25
}

-- | The underlying function for the 'cases' tactic.
cases :: SynthesisState -> Maybe (Expression, Substitution)
cases ss =
  let
    ix = freshCounter ss
  in
    case holeData ss of
      Just (_, t) -> Just (ToIfte ix B (ix + 1) t (ix + 2) t, empty)
      _ -> Nothing

-- | The 'cases' tactic.
casesTactic :: Tactic
casesTactic = Tactic {
  tacticName = "cases",
  tactic = cases,
  willPropagate = False,
  counterShift = 3,
  ranking = 17
}

-- | The underlying function for the 'genApply' tactic.
genApply :: SynthesisState -> Maybe (Expression, Substitution)
genApply ss =
  let
    ix = freshCounter ss
  in
    case holeData ss of
      Just (_, t) -> Just (ToApp ix (Func (UVar (ix + 1)) t) (ix + 2) (UVar (ix + 1)), empty)
      _ -> Nothing

-- | The 'genApply' tactic.
genApplyTactic :: Tactic
genApplyTactic = Tactic {
  tacticName = "genApply",
  tactic = genApply,
  willPropagate = False,
  counterShift = 3,
  ranking = 15
}

-- | The underlying function for the 'varGlobal' tactic.
varGlobal :: String -> SynthesisState -> Maybe (Expression, Substitution)
varGlobal varName ss = do
  (_, holeType) <- holeData ss
  varType <- Map.lookup varName (globalCtxt ss)
  let freshType = uSub (freshCounter ss) varType
  subst <- unify [(freshType, holeType)]
  return (ToVar varName, subst)

-- | The 'varGlobal' tactic.
varGlobalTactic :: Int -> (String -> Tactic)
varGlobalTactic shift varName = Tactic {
  tacticName = "varGlobal " ++ varName,
  tactic = varGlobal varName,
  willPropagate = True,
  counterShift = shift,
  ranking = 20
}

-- | The underlying function for the 'varLocal' tactic.
varLocal :: String -> SynthesisState -> Maybe (Expression, Substitution)
varLocal varName ss = do
  (_, holeType) <- holeData ss
  varType <- Map.lookup varName (localCtxt ss)
  subst <- unify [(varType, holeType)]
  return (ToVar varName, subst)

-- | The 'varLocal' tactic.
varLocalTactic :: String -> Tactic
varLocalTactic varName = Tactic {
  tacticName = "varLocal " ++ varName,
  tactic = varLocal varName,
  willPropagate = False,
  counterShift = 0,
  ranking = 40
}

-- | The underlying function for the 'int' tactic.
int :: Int -> SynthesisState -> Maybe (Expression, Substitution)
int n ss =
  case holeData ss of
    Just (_, t) ->
      case unify [(t, Z)] of
        Just subst -> Just (ToNum n, subst)
        _ -> Nothing
    _ -> Nothing 

-- | The 'int' tactic.
intTactic :: Int -> Tactic
intTactic n = Tactic {
  tacticName = "int " ++ show n,
  tactic = int n,
  willPropagate = True,
  counterShift = 0,
  ranking = 7
}

-- | The underlying function for the 'bool' tactic.
bool :: Bool -> SynthesisState -> Maybe (Expression, Substitution)
bool b ss =
  case holeData ss of
    Just (_, t) ->
      case unify [(t, B)] of
        Just subst -> Just (ToBool b, subst)
        _ -> Nothing
    _ -> Nothing

-- | The 'bool' tactic.
boolTactic :: Bool -> Tactic
boolTactic b = Tactic {
  tacticName = "bool " ++ (if b then "true" else "false"),
  tactic = bool b,
  willPropagate = True, 
  counterShift = 0,
  ranking = 5
}

-- | The underlying function for the 'globalApply' tactic.
globalApply :: String -> SynthesisState -> Maybe (Expression, Substitution)
globalApply funcName ss = do
  (_, holeType) <- holeData ss
  funcType <- Map.lookup funcName (globalCtxt ss)
  
  let ix = freshCounter ss
  let freshType = uSub ix funcType
  let shift = countTypeVars freshType
  (outType, paramTypes) <- splitFunctionType1 freshType
  subst <- unify [(outType, holeType)]

  let newParamTypes = List.map (applySubst subst) paramTypes
  return (ToApps funcName (ix + shift) newParamTypes, subst)

-- | The 'globalApply' tactic.
globalApplyTactic :: Int -> (String -> Tactic)
globalApplyTactic shift funcName = Tactic {
  tacticName = "globalApply " ++ funcName,
  tactic = globalApply funcName,
  willPropagate = True,
  counterShift = shift,
  ranking = 21
}

-- | The underlying function for the 'localApply' tactic.
localApply :: String -> SynthesisState -> Maybe (Expression, Substitution)
localApply funcName ss = do
  (_, holeType) <- holeData ss
  funcType <- Map.lookup funcName (localCtxt ss)
  (outType, paramTypes) <- splitFunctionType1 funcType
  subst <- unify [(outType, holeType)]

  let ix = freshCounter ss
  let newParamTypes = List.map (applySubst subst) paramTypes
  return (ToApps funcName ix newParamTypes, subst)

-- | The 'localApply' tactic.
localApplyTactic :: Int -> (String -> Tactic)
localApplyTactic shift funcName = Tactic {
  tacticName = "localApply " ++ funcName,
  tactic = localApply funcName,
  willPropagate = False,
  counterShift = shift,
  ranking = 21
}