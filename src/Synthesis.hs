module Synthesis where

import AST
import Control.Monad.State
import Data.Map as Map
import Data.Set as Set

data SynthesisState = SynthesisState {
  prog :: Program, -- The zipper.
  global :: Ctxt, -- The context of predefined functions.
  freshCounter :: Int -- A counter to generate fresh vars.
}

-- | A substitution is a map from unification type variable
-- indices to types.
type Substitution = Map Int Type

-- | A constraint is a list of equations between types
-- that must be satisfied.
type Constraint = [(Type, Type)]

-- | Find all unification variable indices present in a type.
findUnifVars :: Type -> Set Int
findUnifVars t =
  case t of
    Prod t1 t2 -> Set.union (findUnifVars t1) (findUnifVars t2)
    Func t1 t2 -> Set.union (findUnifVars t1) (findUnifVars t2)
    List t1 -> findUnifVars t1
    UVar k -> Set.singleton k
    _ -> Set.empty

-- | Apply a substitution to a type.
applySubst :: Substitution -> Type -> Type
applySubst s t =
  case t of
    Prod t1 t2 -> Prod (applySubst s t1) (applySubst s t2)
    Func t1 t2 -> Func (applySubst s t1) (applySubst s t2)
    List t1 -> List (applySubst s t1)
    UVar k ->
      case Map.lookup k s of
        Just t' -> t'
        _ -> t
    _ -> t

-- | Apply substitution to a set of constraints.
substConstraints :: Substitution -> Constraint -> Constraint
substConstraints s [] = []
substConstraints s ((c1, c2) : cs) =
  (applySubst s c1, applySubst s c2) : (substConstraints s cs)

-- | Compose two substitutions; composeSubst s1 s2 is the substitution
-- equivalent to first applying s2 and then s1.
composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s1 s2 =
  let
    s2' = Map.map (applySubst s1) s2
    s1' = filterWithKey (\k _ -> not $ Map.member k s2) s1
  in
    Map.union s1' s2'


unify :: Constraint -> Maybe Substitution
unify [] = Just Map.empty
unify (c : cs) =
  case c of
    (t, UVar k) -> unify $ (UVar k, t) : cs
    (UVar k, t) ->
      if Set.member k (findUnifVars t)
        then Nothing
        else
          let
            newConstraints = substConstraints (Map.singleton k t) cs
          in
            case unify newConstraints of
              Nothing -> Nothing
              Just subst -> Just $ composeSubst subst (Map.singleton k t)
    (Prod t1 t2, Prod t1' t2') -> unify $ (t1, t1') : (t2, t2') : cs
    (Func t1 t2, Func t1' t2') -> unify $ (t1, t1') : (t2, t2') : cs
    (List t1, List t1') -> unify $ (t1, t1') : cs
    (t1, t2) -> 
      if t1 == t2
        then unify cs
        else Nothing


    





{-
TO-DO: Define elementary actions, namely:
1. State-aware unification
2. Propagation of a substitution.
3. Elementary navigation.
4. Elementary replacement of leaves with trees.
-}

-- | Unification of types:
-- Obtain a substitution that unifies two types.
