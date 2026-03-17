{-# LANGUAGE FlexibleInstances #-}
module AST where

import Zipper
import Data.Map (Map)

data Type =
   B              |
   Z              |
   Prod Type Type |
   Func Type Type |
   List Type      |
   TypeVar String |
   UVar Int

type Ctxt = Map String Type

data NodeInfo =
  Var String         |
  Hole Int Type      |
  Lambda String Type |
  App                |
  Pair               |
  Ifte 

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

-- To-do: Propagating a substitution throughout a top-level.
-- To-do: Unification of types.
-- To-do: Functions to get global and local contexts.

type Program = ListZipper TopLevel

-- | Given a tree-zipper, returns tree-zippers to
-- all holes found at or below the focus, along with indices.
-- Assumes the following invariant: there are no holes that are
-- ancestors of other holes.
getHoles :: TreeZipper NodeInfo -> [(Int, TreeZipper NodeInfo)]
getHoles tz =
  case getFocusInfo tz of
    Hole index _ -> [(index, tz)]
    _ ->
      if atLeaf tz
        then []
        else getHolesRight (fullLeft (goDown tz))
  

-- | Auxiliary function for getHoles: obtains all the holes
-- below the focus, and in right-siblings of the focus.
getHolesRight :: TreeZipper NodeInfo -> [(Int, TreeZipper NodeInfo)]
getHolesRight tz =
  if hasRightSibling tz
    then getHoles tz ++ getHolesRight (goRight tz)
    else getHoles tz


-- | Given a tree-zipper, obtain the local context at the focus.
getLocal :: TreeZipper NodeInfo -> Ctxt
getLocal tz =
  let
    ancestorInfo = map (\(TreeCtxt _ x _) -> x) (path tz)
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
      TypeVar s -> show s
      UVar k -> "?" ++ show k

-- | Auxiliary functions for pretty-printing expressions.
showFirst :: [t] -> String 
showFirst xs =
  case xs !? 0 of
    Just x -> show x
    Nothing -> ""

showSecond :: [t] -> String
showSecond xs =
  case xs !? 1 of
    Just x -> show x
    Nothing -> ""

showThird :: [t] -> String
showThird xs =
  case xs !? 2 of
    Just x -> show x
    Nothing -> ""

tab :: String -> String
tab s = unlines $ map (" " ++) $ lines s

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
  show expr = show (toRoot expr)

-- | Given a full top-level expression, obtains a pretty-printed representation.
instance Show TopLevel where
  show tl =
    let
      defName = if isRec tl then "letrec" else "let"
    in
      defName ++ " (" ++ name tl ++ " : " ++ tlType tl ++ ") =\n" ++ (tab $ show expr) ++ "\n"

-- | Given a program, obtain a pretty-printed representation.
instance Show Program where
  show prog = unlines $ map show (toList prog)