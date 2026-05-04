module Parsing where

import Data.Maybe
import qualified Data.Map as MP
import qualified Data.Set as ST
import Data.List

import AST
import StateHandling

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Data.Functor
import Control.Monad.State
import Control.Applicative (empty)

-- | Integer parsing utilities

-- | Given a character, obtains an integer
-- if this character is a digit.
charToDigit :: Char -> Maybe Int
charToDigit c =
  case c of
    '0' -> Just 0
    '1' -> Just 1
    '2' -> Just 2
    '3' -> Just 3
    '4' -> Just 4
    '5' -> Just 5
    '6' -> Just 6
    '7' -> Just 7
    '8' -> Just 8
    '9' -> Just 9
    _ -> Nothing

-- | Given a string, obtains an integer
-- if this string represents an integer.
strToInt :: String -> Maybe Int
strToInt s =
  case s of
    "" -> Just 0
    c : cs ->
      case charToDigit c of
        Just d ->
          case strToInt cs of
            Just n -> Just ((10 ^ (length cs)) * d + n)
            Nothing -> Nothing
        Nothing -> Nothing

parseInteger :: Parser Int
parseInteger = do
  spaces
  num <- many digit
  case strToInt num of
    Just n -> return n
    Nothing -> fail "Couldn't parse integer!"

str :: String -> Parser String
str s = do
  spaces
  t <- string s
  spaces
  return t

parseBool :: Parser Bool
parseBool = do
  spaces
  b <- many alphaNum
  spaces
  case b of
    "true" -> return True
    "false" -> return False
    _ -> fail "Couldn't parse boolean!"


{-
======================================================
|                  PARSING OF TYPES                  |
======================================================
-}

data TypeAST =
  PZ | PB | PList TypeAST | PFunc TypeAST TypeAST
  | PProd TypeAST TypeAST | PVar String | PParens TypeAST
  deriving (Eq, Show)

typeParser :: Parser TypeAST
typeParser = buildExpressionParser table pTypeTerm

opParser :: String -> t -> Parser t
opParser name f = do
  _ <- str name
  return f

table = [ [Infix (try $ opParser "*" PProd) AssocLeft],
          [Infix (try $ opParser "->" PFunc) AssocRight]]

pTypeTerm :: Parser TypeAST
pTypeTerm = pInt <|> pBool <|> pVar <|> pList <|> pParens

pInt :: Parser TypeAST
pInt = str "Int" $> PZ

pBool :: Parser TypeAST
pBool = str "Bool" $> PB

pVar :: Parser TypeAST
pVar = do
  c <- letter 
  return (PVar [c])

pParens :: Parser TypeAST
pParens = PParens <$> between (str "(") (str ")") typeParser

pList :: Parser TypeAST
pList = PList <$> between (str "[") (str "]") typeParser

astToType :: TypeAST -> Type
astToType t =
  case t of
    PZ -> Z
    PB -> B
    PList t' -> List (astToType t')
    PFunc a b -> Func (astToType a) (astToType b)
    PProd a b -> Prod (astToType a) (astToType b)
    PVar s -> TypeVar s
    PParens t' -> astToType t'

getType :: String -> Maybe Type
getType t = 
  case parse typeParser "" t of
    Left _ -> Nothing
    Right ast -> Just $ astToType ast

{-
PARSING OF ACTIONS
-}

-- | Parses the next alphabetic word,
-- assuming no leading whitespace.
nextWord :: Parser String
nextWord = do
  spaces
  many letter

parseAction :: Parser Action
parseAction = do
  action <- nextWord
  case action of
    "let" -> do
      name <- nextWord
      spaces
      ast <- typeParser
      return (Let name (astToType ast))
    "letrec" -> do
      name <- nextWord
      spaces
      ast <- typeParser
      return (Letrec name (astToType ast))
    "jump" -> do
      spaces
      n <- parseInteger
      return (Jump n)
    "exit" -> do
      return Exit
    "intro" -> do
      name <- nextWord
      spaces
      return (Intro name)
    "cases" -> do
      return Cases
    "genApply" -> do
      return GenApply
    "varLocal" -> do
      name <- nextWord
      return (VarLocal name)
    "varGlobal" -> do
      name <- nextWord
      return (VarGlobal name)
    "groundContext" ->
      return GroundContext
    "getFocus" ->
      return GetFocus
    "descendFocus" ->
      return DescendFocus
    "ascendFocus" ->
      return AscendFocus
    "goLeft" ->
      return GoLeft
    "goRight" ->
      return GoRight
    "int" -> do
      n <- parseInteger
      return (IntAction n)
    "bool" -> do
      b <- parseBool
      return (BoolAction b)
    "localApply" -> do
      name <- nextWord
      return (LocalApply name)
    "globalApply" -> do
      name <- nextWord
      return (GlobalApply name)
    _ -> fail "Unknown command!"


getAction :: String -> Action
getAction a =
  case parse parseAction "" a of
    Left _ -> UnknownAction
    Right a' -> a'