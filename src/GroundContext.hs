module GroundContext where

import AST
import Parsing
import Data.Map as Map

-- | To modify the ground context for the synthesis,
-- it suffices to modify this list.
unparsedGroundContext :: [(String, String)]
unparsedGroundContext =
  [
    ("nil", "[t]"),
    ("cons", "t -> [t] -> [t]"),
    ("head", "[t] -> t"),
    ("tail", "[t] -> [t]"),
    ("isNil", "[t] -> Bool"),
    ("leq", "Int -> Int -> Bool"),
    ("eq", "t -> t -> Bool"),
    ("pair", "a -> b -> (a * b)"),
    ("fst", "(a * b) -> a"),
    ("snd", "(a * b) -> b"),
    ("add", "Int -> Int -> Int"),
    ("sub", "Int -> Int -> Int")
  ]

-- | Filters an unparsed context, to include only
-- the typings that could be properly parsed.
parseContext :: [(String, String)] -> [(String, Type)]
parseContext ps =
  case ps of
    [] -> []
    (n, t) : ps' ->
      case getType t of
        Nothing -> parseContext ps'
        Just t' -> (n, t') : (parseContext ps')

-- | The parsed ground context.
groundContext :: Ctxt
groundContext = Map.fromList (parseContext unparsedGroundContext)

