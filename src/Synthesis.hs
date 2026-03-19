module Synthesis where

import AST
import Control.Monad.State

data SynthesisState = SynthesisState {
  prog :: Program, -- The zipper.
  global :: Ctxt, -- The context of predefined functions.
  freshCounter :: Int -- A counter to generate fresh vars.
}

type Substitution = Map Int Type

-- | 
type Unification a = StateT Int Maybe a

getFresh :: Unification Type
getFresh = do
  counter <- get
  put (counter + 1)
  return (UVar counter)

failUnification :: Unification a
failUnification = lift Nothing 

unify :: Type -> Type -> Unification Substitution
unify t1 t2 =


{-
TO-DO: Define elementary actions, namely:
1. State-aware unification
2. Propagation of a substitution.
3. Elementary navigation.
4. Elementary replacement of leaves with trees.
-}

-- | Unification of types:
-- Obtain a substitution that unifies two types.
