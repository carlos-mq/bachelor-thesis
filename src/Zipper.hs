{-|
Module      : Zipper
Description : Defines the core zipper data structures.

This module implements the relevant zipper functionality
for navigating around the program AST; in particular, list-zippers and tree-zippers.
-}
module Zipper where 

-- | The list-zipper
-- A zipper for lists; either empty, or consisting of
-- a focus, along with a list of elements to the left
-- of the focus, and a list of elements to the right.
data ListZipper t = Empty | ListZipper [t] t [t]

empty :: ListZipper t

toListZipper :: [t] -> ListZipper t

goLeft :: ListZipper t -> ListZipper t

goRight :: ListZipper t -> ListZipper t

fullLeft :: ListZipper t -> ListZipper t

fullRight :: ListZipper t -> ListZipper t

prepend :: ListZipper t -> ListZipper t

append :: ListZipper t -> ListZipper t

put :: t -> ListZipper t -> ListZipper t

-- | Tree t: A node with information of type t,
-- along with a list-zipper of its children.
data Tree t = Tree t ListZipper (Tree t)

-- | TreeCtxt t: The internal information of
-- a node, along with its left-siblings and
-- right-siblings.
data TreeCtxt t = TreeCtxt [Tree t] t [Tree t]

type Path t = [TreeCtxt t]

-- | The tree-zipper
-- A zipper for trees; consists of a list-zipper
-- of trees (representing the list of siblings
-- currently being navigated), along with a path;
-- a list of TreeCtxt representing the contexts of
-- all the ancestors all the way to the root.
data TreeZipper t = TreeZipper {
  siblingZipper :: ListZipper (Tree t),
  path :: Path t
}

toTreeZipper :: Tree t -> TreeZipper t

goLeft :: TreeZipper t -> TreeZipper t

goRight :: TreeZipper t -> TreeZipper t

goDown :: TreeZipper t -> TreeZipper t

goUp :: TreeZipper t -> TreeZipper t

toRoot :: TreeZipper t -> Tree t

put :: (Tree t) -> TreeZipper t -> TreeZipper