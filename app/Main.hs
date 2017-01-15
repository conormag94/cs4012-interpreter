module Main where

import Lib
import InterpreterBase

import System.Environment

main :: IO ()
main = do
  let fname = "./program.txt"
  runRun $ startInterpreter fname
  putStrLn "Exiting"
