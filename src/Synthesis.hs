module Synthesis where

import AST
import Control.Monad.State
import Data.Map as Map
import Data.Set as Set
import Zipper

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

-- | Given a set of constraints (type equations), obtain a substitution
-- that solves this set of constraints.
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

-- | Apply a type substitution to a node.
nodeSubst :: Substitution -> NodeInfo -> NodeInfo
nodeSubst s node =
  case node of
    Hole k t -> Hole k (applySubst s t)
    Lambda var t -> Lambda var (applySubst s t)
    _ -> node

-- | Given a tree-zipper and a substitution, propagates the substitution through the tree.
propagateSubst :: Substitution -> TreeZipper NodeInfo -> TreeZipper NodeInfo
propagateSubst s = fmap (nodeSubst s)


{-
Elementary actions
-}

-- | Checks whether it's the last definition.
isLast :: SynthesisState -> Bool
isLast ss = not $ hasRight $ prog ss

-- | Action: go to the previous definition.
prevDefn :: SynthesisState -> SynthesisState
prevDefn ss = ss { prog = goLeft $ prog ss }

-- | Action: go to the next definition.
nextDefn :: SynthesisState -> SynthesisState
nextDefn ss = ss { prog = goRight $ prog ss }

-- | Action: go to the first definition.
firstDefn :: SynthesisState -> SynthesisState
firstDefn ss = ss { prog = fullLeft $ prog ss }

-- | Action: Get all holes in the current definition.
getLocalHoles :: SynthesisState -> [(Int, TreeZipper NodeInfo)]
getLocalHoles ss =
  case getFocus (prog ss) of
    Nothing -> []
    Just tp -> getHoles (expr tp)


-- | Action: replace the current definition in focus
-- with another, if possible.
replaceDefn :: TreeZipper NodeInfo -> SynthesisState -> SynthesisState
replaceDefn tz ss = 
  case getFocus (prog ss) of
    Nothing -> ss
    Just tp -> ss { prog = Zipper.put (replaceExpr tz tp) (prog ss) }


-- | Action: switch to a hole with a particular index, if possible.
switchHole :: Int -> SynthesisState -> Maybe SynthesisState
switchHole k ss =
  let
    s1 = firstDefn ss
  in
    switchHoleRight k s1
  where
    switchHoleRight k ss' =
      case Prelude.lookup k $ getLocalHoles ss' of
        Just tz -> Just (replaceDefn tz ss')
        Nothing ->
          if isLast ss'
            then Nothing
            else switchHoleRight k (nextDefn ss')

-- | Action: go to any hole, if it exists.
anyHole :: SynthesisState -> Maybe SynthesisState
anyHole ss =
  let
    s1 = firstDefn ss
  in
    anyHoleRight s1
  where
    anyHoleRight ss' =
      case getLocalHoles ss' of
        (_, tz) : _ -> Just (replaceDefn tz ss')
        [] ->
          if isLast ss'
            then Nothing
            else anyHoleRight (nextDefn ss')

-- | Action: get the index of the current hole in focus.
currentHole :: SynthesisState -> Maybe Int
currentHole ss =
  case getFocus (prog ss) of
    Nothing -> Nothing
    Just tp ->
      case getFocusInfo (expr tp) of
        Just (Hole k _) -> Just k
        _ -> Nothing
    
-- | Defines a new top-level 'let' at the end of the program,
-- given its name and its type.
newLet :: String -> Type -> SynthesisState -> SynthesisState
newLet n t ss =
  ss { prog = append (letDefn n t (freshCounter ss)) (prog ss),
       freshCounter = (freshCounter ss) + 1 }

-- | Defines a new top-level 'letrec' at the end of the program,
-- given its name and its type.
newLetrec :: String -> Type -> SynthesisState -> SynthesisState
newLetrec n t ss =
  ss { prog = append (letrecDefn n t (freshCounter ss)) (prog ss),
       freshCounter = (freshCounter ss) + 1 }



{-
TO-DO: Define elementary actions, namely:
4. Elementary replacement of leaves with trees.
-}