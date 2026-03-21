module Main where

import Zipper
import AST
import StateHandling
import Parsing
import Tactic
import System.IO
import qualified Data.Map as Map

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
    Exit -> return ()

display :: SynthesisState -> IO ()
display ss = do
  putStrLn (show ss)
  readAction ss

main :: IO ()
main = do
  readAction (initialState Map.empty)
