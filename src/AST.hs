{-|
Module      : AST
Description : Here, everything related to the abstract syntax tree is defined; this includes representations for types, expressions (with heavy usage of the previously defined zippers) and programs. Auxiliary functions for them are also defined.
-}
module AST where

import Zipper
import Data.Map as Map
import Data.List as List
import Data.Set as Set

-- * Types

-- | Representation of types.
data Type =
   B              |
   Z              |
   Prod Type Type |
   Func Type Type |
   List Type      |
   TypeVar String |
   UVar Int
   deriving Eq

-- | Given a type, returns a set of all its type variables.
typeVars :: Type -> Set String
typeVars t =
  case t of
    Prod t1 t2 -> Set.union (typeVars t1) (typeVars t2)
    Func t1 t2 -> Set.union (typeVars t1) (typeVars t2)
    List t1 -> typeVars t1
    TypeVar s -> Set.singleton s
    _ -> Set.empty

-- | Given a type, counts the number of distinct type variables.
countTypeVars :: Type -> Int
countTypeVars t = Set.size (typeVars t)

-- | Given a maybe-type, counts the number of distinct type variables;
-- if Nothing, defaults to 0.
maybeCountTypeVars :: Maybe Type -> Int
maybeCountTypeVars =  maybe 0 countTypeVars


-- | Given a type and a starting index, provides a map of
-- type variable names to appropriate indices.
indexTypeVars :: Int -> Type -> Map String Int
indexTypeVars ix t =
  Map.fromList $ zip (Set.toList (typeVars t)) [ix..]

-- | Substitutes all type variables with unification variables,
-- given a map of names to indices.
tVarToUVar :: Map String Int -> Type -> Type
tVarToUVar varMap t =
  case t of
    TypeVar s ->
      case Map.lookup s varMap of
        Nothing -> t
        Just k -> UVar k
    Prod t1 t2 -> Prod (tVarToUVar varMap t1) (tVarToUVar varMap t2)
    Func t1 t2 -> Func (tVarToUVar varMap t1) (tVarToUVar varMap t2)
    List t1 -> List (tVarToUVar varMap t1)
    _ -> t

-- | Given a type and a starting index,
-- substitutes all type variables with unification variables
-- of appropriate index; also returns the resulting index shift.
uSub :: Int -> Type -> Type
uSub ix t = tVarToUVar (indexTypeVars ix t) t

-- | Splits a function type t1 -> ... -> tn -> t0
-- to (t0, [t1, ..., tn]).
splitFunctionType :: Type -> (Type, [Type])
splitFunctionType t =
  case t of
    Func headType tailType ->
      let
        (p, ps) = splitFunctionType tailType
      in
        (p, headType : ps)
    _ -> (t, [])

-- | Splits a function type t1 -> ... -> tn -> t0,
-- only if there's at least one parameter.

splitFunctionType1 :: Type -> Maybe (Type, [Type])
splitFunctionType1 t =
  case splitFunctionType t of
    (_, []) -> Nothing
    (outType, paramTypes) -> Just (outType, paramTypes)

-- | Counts the number of parameters of the function.
countParams :: Type -> Int
countParams t = length $ snd $ splitFunctionType t

-- * Representation of Expressions and Programs

type Ctxt = Map String Type

-- | Internal data for each node of the tree-zipper representing the program.
data NodeInfo =
  Var String         |
  Hole Int Type      |
  Lambda String Type |
  App                |
  Pair               |
  Ifte               |
  Num Int            |
  Boolean Bool
  deriving (Show)

-- | Elementary actions for replacing the focus
-- of an expression (tree-zipper)
data ReplaceFocus =
  ToNum Int                         | -- ToNum n: replace with integer of value n
  ToBool Bool                       | -- ToBool b: replace with Boolean of value b
  ToVar String                      | -- ToVar s: replace with variable of name s
  ToLambda String Type Int Type     | -- ToLambda x t1 k t2: replace with \(x : t1) |-> (-#k : t2)
  ToApp Int Type Int Type           | -- ToApp n t1 m t2: replace with (-#n : t1) (-#m : t2)
  ToPair Int Type Int Type          | -- ToPair n t1 m t2: replace with (-#n : t1, -#m : t2)
  ToIfte Int Type Int Type Int Type | -- ToIfte l t1 n t2 m t3: replace with if (-#l : t1) then (-#n : t2) else (-#m : t3)
  ToApps String Int [Type]         -- ToApps f n [t1, ..., tk]: replace with f (-#n : t1) ... (-#(n + k) : tk)

-- | Convert a list of nodes into a list-zipper of singleton trees with
-- these as roots.
nodesToLZ :: [NodeInfo] -> ListZipper (Tree NodeInfo)
nodesToLZ nodes =
  toListZipper $ List.map (\n -> (Tree n Zipper.empty)) nodes

-- | Construct a tree, given the info at the root and the info at the children.
makeTree :: NodeInfo -> [NodeInfo] -> Tree NodeInfo
makeTree rootInfo childrenInfo = Tree rootInfo (nodesToLZ childrenInfo)

-- | Given trees [t1, ..., tn], construct tn  ... t1.
-- Notice: in reverse order!
constructApply :: [Tree NodeInfo] -> Tree NodeInfo
constructApply [] = EmptyTree
constructApply [t] = t
constructApply (t : ts) = Tree App (toListZipper [constructApply ts, t])


-- | Obtain the tree corresponding to a replacement action.
rfToTree :: ReplaceFocus -> Tree NodeInfo
rfToTree rf =
  case rf of
    ToNum n -> makeTree (Num n) []
    ToBool b -> makeTree (Boolean b) []
    ToVar s -> makeTree (Var s) []
    ToLambda x t1 k t2 -> makeTree (Lambda x t1) [Hole k t2]
    ToApp n t1 m t2 -> makeTree App [Hole n t1, Hole m t2]
    ToPair n t1 m t2 -> makeTree Pair [Hole n t1, Hole m t2]
    ToIfte l t1 n t2 m t3 -> makeTree Ifte [Hole l t1, Hole n t2, Hole m t3]
    ToApps f _ [] -> makeTree (Var f) []
    ToApps f n types ->
      let
        args = List.map (\(k, t) -> Hole k t) (zip [n..] types)
        argTrees = List.map (`Tree` Zipper.empty) args
        trees = (makeTree (Var f) []) : argTrees
      in
        constructApply $ reverse trees


-- | Replace the focus of a tree-zipper, given a replacement action.
rfTreeZipper :: ReplaceFocus -> TreeZipper NodeInfo -> TreeZipper NodeInfo
rfTreeZipper rf tz = putTree (rfToTree rf) tz


-- | Encodes a top-level definition,
-- including its name, type annotation, and whether
-- it's recursive or not.
data TopLevel = TopLevel {
  name :: String, -- the name of the definition.
  tlType :: Type, -- the type of the definition.
  expr :: TreeZipper NodeInfo, -- the underlying expression.
  isRec :: Bool -- is the definition recursive?
}

-- | Constructs a top-level 'let',
-- given a name, a type, and the index
-- of its hole.
letDefn :: String -> Type -> Int -> TopLevel
letDefn n t ix = TopLevel {
  name = n,
  tlType = t,
  expr = toTreeZipper (Tree (Hole ix t) Zipper.empty),
  isRec = False
}

-- | Constructs a top-level 'letrec',
-- given a name, a type, and the index
-- of its hole.
letrecDefn :: String -> Type -> Int -> TopLevel
letrecDefn n t ix = TopLevel {
  name = n,
  tlType = t,
  expr = toTreeZipper (Tree (Hole ix t) Zipper.empty),
  isRec = True
}

-- | Replace the expression of a top-level
-- with another.
replaceExpr :: TreeZipper NodeInfo -> TopLevel -> TopLevel
replaceExpr tz tl = tl { expr = tz }

-- | Replace the focus of a top-level, given a replacement action
rfTopLevel :: ReplaceFocus -> TopLevel -> TopLevel
rfTopLevel rf tl = replaceExpr (putTree (rfToTree rf) (expr tl)) tl

-- | Obtain the type annotation of a given top-level.
getTypeAnnotation :: TopLevel -> (String, Type)
getTypeAnnotation tl = (name tl, tlType tl)

type Program = ListZipper TopLevel

-- | Obtain the global context induced by a program.
inducedGlobal :: Program -> Ctxt
inducedGlobal prog =
  Map.fromList $ Zipper.toList (fmap getTypeAnnotation prog)

-- * Utilities for Expressions

-- | Given a tree-zipper, returns tree-zippers to
-- all holes found at or below the focus, along with indices.
-- Assumes the following invariant: there are no holes that are
-- ancestors of other holes.
getHolesBelow :: TreeZipper NodeInfo -> [(Int, TreeZipper NodeInfo)]
getHolesBelow tz =
  case getFocusInfo tz of
    Just (Hole index _) -> [(index, tz)]
    Nothing -> []
    _ ->
      if atLeaf tz
        then []
        else getHolesRight (fullLeft (goDown tz))
  

-- | Auxiliary function for getHoles: obtains all the holes
-- below the focus, and in right-siblings of the focus.
getHolesRight :: TreeZipper NodeInfo -> [(Int, TreeZipper NodeInfo)]
getHolesRight tz =
  if hasRight tz
    then getHolesBelow tz ++ getHolesRight (goRight tz)
    else getHolesBelow tz

-- | Given a tree-zipper, returns tree-zippers to all holes found
-- somewhere in the tree.
getHoles :: TreeZipper NodeInfo -> [(Int, TreeZipper NodeInfo)]
getHoles tz = getHolesBelow $ toTreeZipper $ toRoot tz

-- | Maybe-friendly getHoles.
getHoles' :: Maybe (TreeZipper NodeInfo) -> [(Int, TreeZipper NodeInfo)]
getHoles' mtz =
  case mtz of
    Nothing -> []
    Just tz -> getHoles tz


-- | Given a tree-zipper, obtain the local context at the focus.
getLocal :: TreeZipper NodeInfo -> Ctxt
getLocal tz =
  let
    ancestorInfo = List.map (\(TreeCtxt _ x _) -> x) (path tz)
  in
    Map.fromList (filterLambdas ancestorInfo)
    where
      filterLambdas [] = []
      filterLambdas (p : ps) =
        case p of
          Lambda n t -> (n, t) : (filterLambdas ps)
          _ -> filterLambdas ps

-- | Given a type, obtain a pretty-printed representation.
instance Show Type where
  show t =
    case t of
      Z -> "ℤ"
      B -> "𝔹"
      List t1 -> "[" ++ show t1 ++ "]"
      Func t1 t2 -> show t1 ++ " → " ++ show t2
      Prod t1 t2 -> show t1 ++ " × " ++ show t2
      TypeVar s -> s
      UVar k -> "?" ++ show k
