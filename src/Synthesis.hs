module Synthesis where

import AST

data SynthesisState = SynthesisState {
  prog :: Program, -- The zipper.
  global :: Ctxt, -- The context of predefined functions.
  freshCounter :: Int -- A counter to generate fresh vars.
}