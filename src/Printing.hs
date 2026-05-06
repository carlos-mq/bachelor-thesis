{-# LANGUAGE FlexibleInstances #-}
module Printing where

import AST
import Zipper
import Data.Map as Map
import Data.List as List
import StateHandling

data PrintExpression =
  PNum Int                              |
  PBool Bool                            |
  PVar String                           |
  PHole Int Type                        |
  PLambda String Type PrintExpression   |
  PApp [PrintExpression]                |
  PPair PrintExpression PrintExpression |
  PIfte PrintExpression PrintExpression PrintExpression |
  PNothing

tab :: String -> String
tab s = unlines $ List.map (" " ++) $ lines s

-- | Flattens the applications for better printing; i.e.
-- ((a b) c) is printed as (a b c)
flattenApp :: PrintExpression -> PrintExpression
flattenApp p =
  case p of
    PNothing -> PNothing
    PLambda s t p' -> PLambda s t (flattenApp p')
    PPair p1 p2 -> PPair (flattenApp p1) (flattenApp p2)
    PIfte p1 p2 p3 -> PIfte (flattenApp p1) (flattenApp p2) (flattenApp p3)
    PApp ((PApp ys) : xs) -> PApp (ys ++ xs)
    _ -> p

-- | Helper function for print-expressions, defaulting to PNothing
-- if out of bounds.
indexPrintExpr :: [PrintExpression] -> Int -> PrintExpression
indexPrintExpr [] _ = PNothing
indexPrintExpr (x : _) 0 = x
indexPrintExpr (_ : xs) n = indexPrintExpr xs (n - 1)

-- | Converts an expression in tree form into printable form.
toPrint :: Tree NodeInfo -> PrintExpression
toPrint t =
  case t of
    EmptyTree -> PNothing
    Tree (Num n) _ -> PNum n
    Tree (Boolean b) _ -> PBool b
    Tree (Var s) _ -> PVar s
    Tree (Hole k t) _ -> PHole k t
    Tree (Lambda s t) lz -> 
      case Zipper.toList lz of
        [] -> PLambda s t PNothing
        c : _ -> PLambda s t (toPrint c) 
    Tree App lz ->
      PApp (List.map toPrint (Zipper.toList lz))
    Tree Pair lz ->
      PPair 
        (indexPrintExpr (List.map toPrint (Zipper.toList lz)) 0) 
        (indexPrintExpr (List.map toPrint (Zipper.toList lz)) 1)
    Tree Ifte lz ->
      PIfte
        (indexPrintExpr (List.map toPrint (Zipper.toList lz)) 0) 
        (indexPrintExpr (List.map toPrint (Zipper.toList lz)) 1)
        (indexPrintExpr (List.map toPrint (Zipper.toList lz)) 2)

-- | Converts a 'ReplaceFocus' action into printable form.
rfToPrint :: ReplaceFocus -> PrintExpression
rfToPrint rf =
  case rf of
    ToNum n -> PNum n
    ToBool b -> PBool b
    ToVar s -> PVar s
    ToLambda x t1 k t2 -> PLambda x t1 (PHole k t2)
    ToApp k1 t1 k2 t2 -> PApp [PHole k1 t1, PHole k2 t2]
    ToPair k1 t1 k2 t2 -> PPair (PHole k1 t1) (PHole k2 t2)
    ToIfte l t1 n t2 m t3 -> PIfte (PHole l t1) (PHole n t2) (PHole m t3)
    ToApps f n types -> PApp (PVar f : zipWith PHole [n..] types)

instance Show PrintExpression where
  show p =
    case p of
      PNum n -> show n
      PBool b -> if b then "true" else "false"
      PVar s -> s
      PHole id t -> "(-#" ++ show id ++ " : " ++ show t ++ ")"
      PLambda s t p' -> "λ(" ++ s ++ " : " ++ show t ++ ") ↦ \n" ++ (tab $ show p')
      PApp ps -> "(" ++ tail (concat (List.map (\q -> " " ++ show q) ps)) ++ ")"
      PPair p1 p2 -> "(" ++ show p1 ++ ", " ++ show p2 ++ ")"
      PIfte p1 p2 p3 -> "if " ++ show p1 ++ "\n" ++
        tab ( "then " ++ show p2 ++ "\n"
        ++ "else " ++ show p3)
      PNothing -> ""

instance Show ReplaceFocus where
  show rf = show (rfToPrint rf)


-- | Given a full top-level expression, obtains a pretty-printed representation.
instance Show TopLevel where
  show tl =
    let
      defName = if isRec tl then "letrec" else "let"
    in
      defName ++ " (" ++ name tl ++ " : " ++ show (tlType tl) ++ ") =\n" ++
      tab (show $ flattenApp $ toPrint $ toRoot $ expr tl)

-- | Given a program, obtain a pretty-printed representation.
instance Show Program where
  show prog = unlines $ List.map show (Zipper.toList prog)

showCtxt :: Ctxt -> String
showCtxt ctxt =
  let
    annotations = List.map (\(n, t) -> n ++ " : " ++ (show t) ++ "\n") (Map.toList ctxt)
  in
    "{" ++ concat annotations ++ "}"
