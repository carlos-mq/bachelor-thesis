{-# LANGUAGE FlexibleInstances #-}
module AST where

import Zipper
import Data.Map as Map
import Data.Map.Lazy (fromList)
import Data.List as List
import Data.Set as Set

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


type Ctxt = Map String Type

data NodeInfo =
  Var String         |
  Hole Int Type      |
  Lambda String Type |
  App                |
  Pair               |
  Ifte 

-- | Elementary actions for replacing the focus
-- of an expression (tree-zipper)
data ReplaceFocus =
  ToVar String                      | -- ToVar s: replace with variable of name s
  ToLambda String Type Int Type     | -- ToLambda x t1 k t2: replace with \(x : t1) |-> (-#k : t2)
  ToApp Int Type Int Type           | -- ToApp n t1 m t2: replace with (-#n : t1) (-#m : t2)
  ToPair Int Type Int Type          | -- ToPair n t1 m t2: replace with (-#n : t1, -#m : t2)
  ToIfte Int Type Int Type Int Type  -- ToIfte l t1 n t2 m t3: replace with if (-#l : t1) then (-#n : t2) else (-#m : t3)

-- | Convert a list of nodes into a list-zipper of singleton trees with
-- these as roots.
nodesToLZ :: [NodeInfo] -> ListZipper (Tree NodeInfo)
nodesToLZ nodes =
  toListZipper $ List.map (\n -> (Tree n Zipper.empty)) nodes


-- | Obtain the tree corresponding to a replacement action.
rfToTree :: ReplaceFocus -> Tree NodeInfo
rfToTree rf =
  case rf of
    ToVar s -> Tree (Var s) Zipper.empty
    ToLambda x t1 k t2 -> Tree (Lambda x t1) (nodesToLZ [Hole k t2])
    ToApp n t1 m t2 -> Tree App (nodesToLZ [Hole n t1, Hole m t2])
    ToPair n t1 m t2 -> Tree Pair (nodesToLZ [Hole n t1, Hole m t2])
    ToIfte l t1 n t2 m t3 -> Tree Ifte (nodesToLZ [Hole l t1, Hole n t2, Hole m t3])

-- | Replace the focus of a tree-zipper, given a replacement action.
rfTreeZipper :: ReplaceFocus -> TreeZipper NodeInfo -> TreeZipper NodeInfo
rfTreeZipper rf tz = putTree (rfToTree rf) tz





-- Notice:
-- There's no need to keep track of local contexts
-- in the TopLevel, since these can be quickly
-- recovered from the path in the tree-zipper.

data TopLevel = TopLevel {
  name :: String,
  tlType :: Type,
  expr :: TreeZipper NodeInfo,
  isRec :: Bool
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

getTypeAnnotation :: TopLevel -> (String, Type)
getTypeAnnotation tl = (name tl, tlType tl)

type Program = ListZipper TopLevel

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

-- | Auxiliary functions for pretty-printing expressions.
showFirst :: (Show t) => [t] -> String 
showFirst xs =
  case xs of 
    x : _ -> show x
    _ -> ""

showSecond :: (Show t) => [t] -> String
showSecond xs =
  case xs of
    _ : y : _ -> show y
    _ -> ""

showThird :: (Show t) => [t] -> String
showThird xs =
  case xs of
    _ : _ : z : _ -> show z
    _ -> ""

tab :: String -> String
tab s = unlines $ List.map (" " ++) $ lines s

-- | Given an expression in tree form, obtains a pretty-printed representation.
-- It ignores any nodes below variables and holes.
instance Show (Tree NodeInfo) where
  show expr =
    case getRoot expr of
      Nothing -> ""
      Just (Var s) -> s
      Just (Hole id t) -> "( -#" ++ show id ++ " : " ++ show t ++ ")"
      Just (Lambda s t) -> "λ(" ++ s ++ " : " ++ show t ++ ") ↦ \n" ++ (tab $ fstChild)
      Just App -> "(" ++ fstChild ++ ") (" ++ sndChild ++ ")"
      Just Pair -> "(" ++ fstChild ++ ", " ++ sndChild ++ ")"
      Just Ifte ->
        "if (" ++ fstChild ++ ")\n" ++
        tab ( "then (" ++ sndChild ++ ")\n")
        ++ "else (" ++ thrdChild ++ ")"
    where
      fstChild = showFirst $ getChildrenList expr
      sndChild = showSecond $ getChildrenList expr
      thrdChild = showThird $ getChildrenList expr

-- | Given an expression in tree-zipper form, obtains a pretty-printed representation.
-- Uses the tree form representation from the root.
instance Show (TreeZipper NodeInfo) where
  show e = show (toRoot e)

-- | Given a full top-level expression, obtains a pretty-printed representation.
instance Show TopLevel where
  show tl =
    let
      defName = if isRec tl then "letrec" else "let"
    in
      defName ++ " (" ++ name tl ++ " : " ++ show (tlType tl) ++ ") =\n" ++ tab (show (expr tl)) ++ "\n"

{-
Program-wide utilities
-}

-- | Given a program, obtain a pretty-printed representation.
instance Show Program where
  show prog = unlines $ List.map show (Zipper.toList prog)

-- | Obtain the global context induced by a program.
inducedGlobal :: Program -> Ctxt
inducedGlobal prog =
  Map.fromList $ Zipper.toList (fmap getTypeAnnotation prog)

