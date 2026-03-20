module Main where

import Zipper
import AST
import StateHandling
import Parsing
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
    Exit -> return ()

display :: SynthesisState -> IO ()
display ss = do
  putStrLn (show ss)
  readAction ss

main :: IO ()
main = do
  readAction (initialState Map.empty)
