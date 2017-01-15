{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-} 
{-# LANGUAGE FlexibleInstances #-} 

module InterpreterBase where

import Prelude hiding (lookup, print)

import qualified Data.Map as Map
import Data.Maybe

import qualified System.IO as System

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

{-------------------------------------------------------------------}
{- The pure expression language                                    -}
{-------------------------------------------------------------------}

-- http://catamorph.de/documents/Transformers.pdf

data Val = I Int | B Bool
           deriving (Eq, Show, Read)

data Expr = Const Val
     | Add Expr Expr | Sub Expr Expr  | Mul Expr Expr | Div Expr Expr
     | And Expr Expr | Or Expr Expr | Not Expr 
     | Eq Expr Expr | Gt Expr Expr | Lt Expr Expr
     | Var String
   deriving (Eq, Show, Read)

type Name = String 
type Env = Map.Map Name Val

lookup k t = case Map.lookup k t of
               Just x -> return x
               Nothing -> fail ("Unknown variable "++k)

{-- Monadic style expression evaluator, 
 -- with error handling and Reader monad instance to carry dictionary
 --}

type Eval a = ReaderT Env (ExceptT String Identity) a 
runEval env ex = runIdentity ( runExceptT ( runReaderT ex env) )


-- Integer typed expressions

evali op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (I i0, I i1) -> return $ I (i0 `op` i1)
                         _            -> fail "type error in arithmetic expression"

-- Boolean typed expressions

evalb op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (B i0, B i1) -> return $ B (i0 `op` i1)
                         _            -> fail "type error in boolean expression"

-- Operations over integers which produce booleans

evalib op e0 e1 = do e0' <- eval e0
                     e1' <- eval e1
                     case (e0', e1') of
                          (I i0, I i1) -> return $ B (i0 `op` i1)
                          _            -> fail "type error in arithmetic expression"

-- Evaluate an expression

eval :: Expr -> Eval Val
eval (Const v) = return v
eval (Add e0 e1) = do evali (+) e0 e1
eval (Sub e0 e1) = do evali (-) e0 e1
eval (Mul e0 e1) = do evali (*) e0 e1
eval (Div e0 e1) = do evali div e0 e1

eval (And e0 e1) = do evalb (&&) e0 e1
eval (Or e0 e1) = do evalb (||) e0 e1

eval (Not e0  ) = do evalb (const not) e0 (Const (B True)) 
                       where not2 a _ = not a -- hack, hack

eval (Eq e0 e1) = do evalib (==) e0 e1
eval (Gt e0 e1) = do evalib (>) e0 e1
eval (Lt e0 e1) = do evalib (<) e0 e1
                        
eval (Var s) = do env <- ask
                  lookup s env


{-------------------------------------------------------------------}
{- The statement language                                          -}


data Statement = Assign String Expr
               | If Expr Statement Statement
               | While Expr Statement
               | Print Expr
               | Seq Statement Statement
               | Try Statement Statement
               | Pass                    
      deriving (Eq, Show, Read)

type Program = [(Statement)]

type Run a = StateT Env (ExceptT String IO) a 
runRun p =  runExceptT ( runStateT p Map.empty) 

set :: (Name, Val) -> Run ()
set (s,i) = state $ (\table -> ((), Map.insert s i table))

exec :: Statement -> Run ()
exec (Assign s v) = do st <- get  
                       Right val <- return $ runEval st (eval v)  
                       set (s,val)

exec (Seq s0 s1) = do exec s0 >> exec s1

exec (Print e) = do st <- get
                    Right val <- return $ runEval st (eval e) 
                    liftIO $ System.print val
                    return () 

exec (If cond s0 s1) = do st <- get
                          Right (B val) <- return $ runEval st (eval cond)
                          if val then do exec s0 else do exec s1

exec (While cond s) = do st <- get
                         Right (B val) <- return $ runEval st (eval cond)
                         if val then do exec s >> exec (While cond s) else return ()

exec (Try s0 s1) = do catchError (exec s0) (\e -> exec s1)
                        
exec Pass = return ()

-- Executes statements one by one based on user input
-- 'n' to execute next statement
-- 'q' to quit
execStatements :: Env -> [Statement] -> Run ()
execStatements _ [] = do
  liftIO $ putStrLn ">>>>>> Reached end of input program"
  return ()
execStatements env (x:xs) = do
  liftIO $ putStrLn (">> Statement: " ++ (show x))
  userInput <- liftIO $ getLine
  case userInput of 
    "n" -> do
      exec x
      newEnv <- get
      execStatements newEnv xs
    "q" -> do
      return()
    _ -> do
      liftIO $ putStrLn "Unrecognised command"
      execStatements env (x:xs)

-- Convert lines to statements
getStatements :: [String] -> [Statement]
getStatements = map read


-- Entry point
-- Reads file and sets up initial env and statements list
startInterpreter :: String -> Run ()
startInterpreter fname = do 
  fileHandle <- liftIO $ readFile fname
  let fileLines = lines fileHandle
  let theProgram = getStatements fileLines               -- Execute the first statement
  env <- get
  liftIO $ putStrLn "** Interpreter started **"
  liftIO $ putStrLn "'n' to execute next statement. 'q' to quit"
  execStatements env theProgram


prog1 = Assign "foo" (Const (I 1))  `Seq`
        Print (Var "foo")

prog2 = Print (Const (I 1)) `Seq`
        Print (Const (I 2))
