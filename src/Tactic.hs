module Tactic where

import AST
import StateHandling
import Data.Map as Map
import qualified Data.List as List

type Expression = ReplaceFocus

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

-- | The 'intro' tactic
-- To-do: Add a check to see if the variable name is already in the context.
intro :: String -> SynthesisState -> Maybe (Expression, Substitution)
intro var ss =
  let
    ix = freshCounter ss
  in
    case holeData ss of
      Just (_, Func t1 t2) -> Just (ToLambda var t1 ix t2, empty)
      _ -> Nothing

introTactic :: String -> Tactic
introTactic var = Tactic {
  tacticName = "intro " ++ var,
  tactic = intro var,
  willPropagate = False,
  counterShift = 1,
  ranking = 25
}

-- | The 'cases' tactic
cases :: SynthesisState -> Maybe (Expression, Substitution)
cases ss =
  let
    ix = freshCounter ss
  in
    case holeData ss of
      Just (_, t) -> Just (ToIfte ix B (ix + 1) t (ix + 2) t, empty)
      _ -> Nothing

casesTactic :: Tactic
casesTactic = Tactic {
  tacticName = "cases",
  tactic = cases,
  willPropagate = False,
  counterShift = 3,
  ranking = 17
}

-- | The 'general apply' tactic
genApply :: SynthesisState -> Maybe (Expression, Substitution)
genApply ss =
  let
    ix = freshCounter ss
  in
    case holeData ss of
      Just (_, t) -> Just (ToApp ix (Func (UVar (ix + 1)) t) (ix + 2) (UVar (ix + 1)), empty)
      _ -> Nothing

genApplyTactic :: Tactic
genApplyTactic = Tactic {
  tacticName = "genApply",
  tactic = genApply,
  willPropagate = False,
  counterShift = 3,
  ranking = 15
}

-- | The 'var' tactics

-- | VarGlobal requires knowing the counter shift beforehand,
-- since it depends on the actual type of the parameter.
{-
varGlobal :: String -> SynthesisState -> Maybe (Expression, Substitution)
varGlobal varName ss =
  case holeData ss of
    Just (_, t) ->
      case Map.lookup varName (globalCtxt ss) of
        Just varType ->
          let
            freshType = uSub (freshCounter ss) varType
          in
            case unify [(freshType, t)] of
              Just subst -> Just (ToVar varName, subst)
              _ -> Nothing
        _ -> Nothing
    _ -> Nothing
-}

varGlobal :: String -> SynthesisState -> Maybe (Expression, Substitution)
varGlobal varName ss = do
  (_, holeType) <- holeData ss
  varType <- Map.lookup varName (globalCtxt ss)
  let freshType = uSub (freshCounter ss) varType
  subst <- unify [(freshType, holeType)]
  return (ToVar varName, subst)



varGlobalTactic :: Int -> (String -> Tactic)
varGlobalTactic shift varName = Tactic {
  tacticName = "varGlobal " ++ varName,
  tactic = varGlobal varName,
  willPropagate = True,
  counterShift = shift,
  ranking = 20
}

{-
varLocal :: String -> SynthesisState -> Maybe (Expression, Substitution)
varLocal varName ss =
  case holeData ss of
    Just (_, t) ->
      case Map.lookup varName (localCtxt ss) of
        Just varType ->
          case unify [(varType, t)] of
            Just subst -> Just (ToVar varName, subst)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
-}

varLocal :: String -> SynthesisState -> Maybe (Expression, Substitution)
varLocal varName ss = do
  (_, holeType) <- holeData ss
  varType <- Map.lookup varName (localCtxt ss)
  subst <- unify [(varType, holeType)]
  return (ToVar varName, subst)


varLocalTactic :: String -> Tactic
varLocalTactic varName = Tactic {
  tacticName = "varLocal " ++ varName,
  tactic = varLocal varName,
  willPropagate = False,
  counterShift = 0,
  ranking = 40
}

int :: Int -> SynthesisState -> Maybe (Expression, Substitution)
int n ss =
  case holeData ss of
    Just (_, t) ->
      case unify [(t, Z)] of
        Just subst -> Just (ToNum n, subst)
        _ -> Nothing
    _ -> Nothing 

intTactic :: Int -> Tactic
intTactic n = Tactic {
  tacticName = "int " ++ show n,
  tactic = int n,
  willPropagate = True,
  counterShift = 0,
  ranking = 7
}

bool :: Bool -> SynthesisState -> Maybe (Expression, Substitution)
bool b ss =
  case holeData ss of
    Just (_, t) ->
      case unify [(t, B)] of
        Just subst -> Just (ToBool b, subst)
        _ -> Nothing
    _ -> Nothing

boolTactic :: Bool -> Tactic
boolTactic b = Tactic {
  tacticName = "bool " ++ (if b then "true" else "false"),
  tactic = bool b,
  willPropagate = True, 
  counterShift = 0,
  ranking = 5
}

-- The globalApply and localApply tactics

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

globalApplyTactic :: Int -> (String -> Tactic)
globalApplyTactic shift funcName = Tactic {
  tacticName = "globalApply " ++ funcName,
  tactic = globalApply funcName,
  willPropagate = True,
  counterShift = shift,
  ranking = 21
}

localApply :: String -> SynthesisState -> Maybe (Expression, Substitution)
localApply funcName ss = do
  (_, holeType) <- holeData ss
  funcType <- Map.lookup funcName (localCtxt ss)
  (outType, paramTypes) <- splitFunctionType1 funcType
  subst <- unify [(outType, holeType)]

  let ix = freshCounter ss
  let newParamTypes = List.map (applySubst subst) paramTypes
  return (ToApps funcName ix newParamTypes, subst)

localApplyTactic :: Int -> (String -> Tactic)
localApplyTactic shift funcName = Tactic {
  tacticName = "localApply " ++ funcName,
  tactic = localApply funcName,
  willPropagate = False,
  counterShift = shift,
  ranking = 21
}