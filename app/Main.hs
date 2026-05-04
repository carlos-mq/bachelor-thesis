module Main where

import Zipper
import AST
import StateHandling
import Parsing
import Tactic
import GroundContext
import Printing
import System.IO
import Complete
import qualified Data.Map as Map

-- Maximum number of suggested tactics to show.
suggestionLimit :: Int 
suggestionLimit = 6

{-
Represents the output of the 'complete' function
in a readable way.
-}
showComplete :: [(Tactic, ReplaceFocus)] -> String
showComplete out =
  let
    indexed = zip [1..suggestionLimit] out
  in
    concatMap (\(n, (tac, rf)) -> (show n) ++ ". " ++ (show rf) ++ " [ " ++ (tacticName tac) ++ " ]\n") indexed

instance Show SynthesisState where
  show ss =
    let
      completeList = showComplete (complete ss (tacticList ss))
    in 
    "\n" ++ "Current hole: " ++ (showCurrentHole ss) ++ "\n \n" ++
    (show $ prog ss) ++ "\n" ++ "Options: \n" ++ completeList

prompt :: String -> IO String
prompt text = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  putStr text
  hFlush stdout
  getLine

readAction :: SynthesisState -> IO ()
readAction ss = do
  input <- prompt "> "
  case getAction input of
    UnknownAction -> do
      putStrLn "Unknown command. Try again."
      readAction ss
    Let n t ->
      display (newLet n t ss)
    Letrec n t ->
      display (newLetrec n t ss)
    Jump k ->
      case (switchHole k ss) of
        Just ss' -> display ss'
        Nothing -> do
          putStrLn "Unknown hole. Try again."
          readAction ss
    Intro x -> -- Later on, add a check that ensures that the variable name isn't in the context already.
      display (runTactic (introTactic x) ss)
    Cases ->
      display (runTactic casesTactic ss)
    GenApply ->
      display (runTactic genApplyTactic ss)
    VarLocal x -> 
      display (runTactic (varLocalTactic x) ss)
    VarGlobal x -> do
      let shift = countTypeVarsInGlobal ss x
      display (runTactic (varGlobalTactic shift x) ss)
    IntAction n -> do
      display (runTactic (intTactic n) ss)
    BoolAction b -> do
      display (runTactic (boolTactic b) ss)
    GroundContext -> do
      putStrLn (showCtxt (groundCtxt ss))
      readAction ss
    GetFocus ->
      case (getFocusInfo $ getProgFocus ss) of
        Just f -> do 
          putStrLn (show f)
          readAction ss
        Nothing -> readAction ss
    DescendFocus ->
      display (descendFocus ss)
    AscendFocus ->
      display (ascendFocus ss)
    GoLeft ->
      display (leftFocus ss)
    GoRight ->
      display (rightFocus ss)
    Exit -> return ()

display :: SynthesisState -> IO ()
display ss = do
  print ss
  readAction ss

main :: IO ()
main = do
  readAction (initialState groundContext)
