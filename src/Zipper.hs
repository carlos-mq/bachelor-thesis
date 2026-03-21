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

-- | Returns an empty list-zipper.
empty :: ListZipper t
empty = Empty

-- | Turns a list into a list-zipper; if the list is
-- non-empty, the focus is in the list's head by default.
toListZipper :: [t] -> ListZipper t
toListZipper xs =
  case xs of 
    [] -> Empty
    y : ys -> ListZipper [] y ys

-- | Turns a list-zipper back into a list.
toList :: ListZipper t -> [t]
toList lz =
  case lz of
    Empty -> []
    ListZipper ls f rs -> (reverse ls) ++ [f] ++ rs

-- | Gives a list-zipper containing only the specified element.
singleton :: t -> ListZipper t
singleton x = toListZipper [x]


-- | A zipper where one can move the focus to the left or the right.
class OneDimensional f where
  -- | Moves the focus one element to the left, if possible.
  goLeft :: f t -> f t
  -- | Moves the focus one element to the right, if possible.
  goRight :: f t -> f t
  -- | Determines whether there's an element to the right.
  hasRight :: f t -> Bool
  -- | Determines whether there's an element to the left.
  hasLeft :: f t -> Bool



-- | Moves the focus to the leftmost possible position.
fullLeft :: (OneDimensional f) => f t -> f t
fullLeft x =
  if hasLeft x
    then fullLeft (goLeft x)
    else x

-- | Moves the focus to the rightmost possible position.
fullRight :: (OneDimensional f) => f t -> f t
fullRight x =
  if hasRight x
    then fullRight (goRight x)
    else x

instance OneDimensional ListZipper where
  goLeft z =
    case z of
      ListZipper ls f rs ->
        case ls of
          [] -> z
          l : ls' -> ListZipper ls' l (f : rs)
      _ -> Empty
  goRight z =
    case z of
      ListZipper ls f rs ->
        case rs of
          [] -> z
          r : rs' -> ListZipper (f : ls) r rs'
      _ -> Empty
  hasRight z =
    case z of
      ListZipper _ _ (_ : _) -> True
      _ -> False
  hasLeft z =
    case z of
      ListZipper (_ : _) _ _ -> True
      _ -> False

instance Functor ListZipper where
  fmap f lz =
    case lz of
      Empty -> Empty
      ListZipper ls c rs -> ListZipper (map f ls) (f c) (map f rs)


-- | Gets the list of elements to the left of the focus.
getLeft :: ListZipper t -> [t]
getLeft z = 
  case z of
    Empty -> []
    ListZipper ls _ _ -> ls

-- | Gets the list of elements to the right of the focus.
getRight :: ListZipper t -> [t]
getRight z =
  case z of
    Empty -> []
    ListZipper _ _ rs -> rs

-- | Gets the focus, if it exists.
getFocus :: ListZipper t -> Maybe t
getFocus z =
    case z of
      Empty -> Nothing
      ListZipper _ x _ -> Just x

-- | Places an element right after the focus, leaving
-- the focus in this element by default.
placeAfter :: t -> ListZipper t -> ListZipper t
placeAfter x zx =
  case zx of
    Empty -> ListZipper [] x []
    ListZipper ls f rs -> ListZipper (f : ls) x rs

-- | Appends an element to the list-zipper,
-- leaving the focus in this element by default.
append :: t -> ListZipper t -> ListZipper t
append x zx = placeAfter x (fullRight zx)

-- Replaces the focus with another element of the same
-- type, if possible; leaving the focus in this element
-- by default. If the list is empty, it stays empty.
put :: t -> ListZipper t -> ListZipper t
put x zx =
  case zx of
    Empty -> Empty
    ListZipper ls _ rs -> ListZipper ls x rs

-- | Tree t: A node with information of type t,
-- along with a list-zipper of its children.
data Tree t = Tree t (ListZipper (Tree t)) | EmptyTree

instance Functor Tree where
  fmap f t =
    case t of
      EmptyTree -> EmptyTree
      Tree x cs -> Tree (f x) (fmap (\tree -> fmap f tree) cs)

-- | Gets the internal information of the root of a tree.
getRoot :: Tree t -> Maybe t
getRoot (Tree x _) = Just x
getRoot _ = Nothing

-- | Gets the list-zipper of the children of the root of a tree.
getChildren :: Tree t -> Maybe (ListZipper (Tree t))
getChildren (Tree _ cs) = Just cs
getChildren _ = Nothing

-- | Gets the children of the root of a tree, but as a list.
getChildrenList :: Tree t -> [Tree t]
getChildrenList t =
  case getChildren t of
    Nothing -> []
    Just lz -> toList lz

-- | TreeCtxt t: The internal information of
-- a node, along with its left-siblings and
-- right-siblings.
data TreeCtxt t = TreeCtxt [Tree t] t [Tree t] | EmptyCtxt

instance Functor TreeCtxt where
  fmap f tc =
    case tc of
      EmptyCtxt -> EmptyCtxt
      TreeCtxt ls c rs -> TreeCtxt (map (fmap f) ls) (f c) (map (fmap f) rs)

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

-- | Gives a tree-zipper for a given tree,
-- with the focus in the root by default.
toTreeZipper :: Tree t -> TreeZipper t
toTreeZipper tree = TreeZipper {
  siblingZipper = singleton tree,
  path = []
}

-- | Gives the context of the current focus; that is,
-- its list of left-siblings, its internal information,
-- and its list of right-siblings.
getContext :: TreeZipper t -> TreeCtxt t
getContext tz =
  case siblingZipper tz of
    ListZipper ls f rs -> 
      case getRoot f of
        Nothing -> EmptyCtxt
        Just root -> TreeCtxt ls root rs
    Empty -> EmptyCtxt

instance OneDimensional TreeZipper where
  -- | Moves the focus to the sibling directly to the
  -- left, if possible.
  goLeft tz = tz { siblingZipper = goLeft (siblingZipper tz) }
  -- | Moves the focus to the sibling directly to the
  -- right, if possible
  goRight tz = tz { siblingZipper = goRight (siblingZipper tz) }
  hasRight tz = hasRight (siblingZipper tz)
  hasLeft tz =  hasLeft (siblingZipper tz)

instance Functor TreeZipper where
  fmap f tz = TreeZipper {
    siblingZipper = fmap (fmap f) (siblingZipper tz),
    path = map (fmap f) (path tz)
  }


-- | Get the subtree at the focus of the tree-zipper.
getFocusSubtree :: TreeZipper t -> Tree t
getFocusSubtree tz = 
  case getFocus (siblingZipper tz) of
    Nothing -> EmptyTree
    Just x -> x

-- | Get the info stored at the focus of the tree-zipper, if it exists.
getFocusInfo :: TreeZipper t -> Maybe t
getFocusInfo tz =
  case getFocusSubtree tz of
    EmptyTree -> Nothing
    Tree info _ -> Just info

-- | Get the list-zipper of the children of the current focus.
getFocusChildren :: TreeZipper t -> ListZipper (Tree t)
getFocusChildren tz =
  case getFocusSubtree tz of
    EmptyTree -> Empty
    Tree _ children -> children

-- | Determines whether the focus is at a leaf.
atLeaf :: TreeZipper t -> Bool
atLeaf tz =
  case getFocusChildren tz of
    Empty -> True
    _ -> False

-- | If possible, move the focus down to the focus of the children.
goDown :: TreeZipper t -> TreeZipper t
goDown tz =
  case getFocus (siblingZipper tz) of
    Nothing -> tz
    Just tree ->
      case getChildren tree of
        Nothing -> tz
        Just cs -> TreeZipper {
          siblingZipper = cs,
          path = getContext tz : path tz
        }

-- | Given a TreeCtxt and a list-zipper of children,
-- builds back a list-zipper focused at the tree.
ctxtToZipper :: TreeCtxt t -> ListZipper (Tree t) -> ListZipper (Tree t)
ctxtToZipper ctxt lz =
  case ctxt of
    EmptyCtxt -> Empty
    TreeCtxt ls info rs -> ListZipper ls (Tree info lz) rs

-- | If possible, move the focus up to the parent.
goUp :: TreeZipper t -> TreeZipper t
goUp tz =
  case path tz of
    [] -> tz
    parentCtxt : path' -> TreeZipper {
      siblingZipper = ctxtToZipper parentCtxt (siblingZipper tz),
      path = path'
    }

-- | Return the whole tree, from the perspective of the root.
toRoot :: TreeZipper t -> Tree t
toRoot tz =
  case path tz of
    [] -> case siblingZipper tz of
      Empty -> EmptyTree
      ListZipper _ tree _ -> tree
    _ -> toRoot (goUp tz)



-- | In a given tree-zipper, replace the current focus with a tree
-- rooted at this focus.
putTree :: Tree t -> TreeZipper t -> TreeZipper t
putTree tree tz = tz {siblingZipper = put tree (siblingZipper tz)}