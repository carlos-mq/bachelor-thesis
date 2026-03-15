module AST where

import Zipper
import Data.Map (Map)

data Type = T String -- Provisional
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
  type' :: Type,
  expr :: TreeZipper NodeInfo,
  isRec :: Bool
}

-- To-do: Propagating a substitution throughout a top-level.
-- To-do: Unification of types.
-- To-do: Functions to get global and local contexts.

type Program = ListZipper TopLevel

-- | Given a tree T, returns tree-zippers to
-- all holes found in T, along with indices.

getHoles :: Tree NodeInfo -> [(Int, TreeZipper NodeInfo)]

-- 