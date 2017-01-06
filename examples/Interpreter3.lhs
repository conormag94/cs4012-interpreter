> module Interpreter3 where
> 
> import Prelude hiding (lookup)
> import qualified Data.Map as Map
> import Data.Maybe

> import Control.Monad.Identity
> import Control.Monad.Except

> data Val = I Int | B Bool
>            deriving Show

> data Expr = Const Val
>      | Add Expr Expr | Sub Expr Expr  | Mul Expr Expr | Div Expr Expr
>      | And Expr Expr | Or Expr Expr | Not Expr 
>      | Eq Expr Expr | Gt Expr Expr | Lt Expr Expr
>      | Var String
>    deriving Show

> type Name = String 
> type Env = Map.Map Name Val

{-- Monadic style, adding error handling --}

> lookup k t = case Map.lookup k t of
>                Just x -> return x
>                Nothing -> fail "lookup: key failed"

> type Eval a = ExceptT String Identity a 
> runEval = runIdentity . runExceptT 

> eval :: Env -> Expr -> Eval Val
> eval _ (Const v) = return v
> eval e (Add e0 e1) = do i0 <- eval e e0
>                         i1 <- eval e e1
>                         return $ I (unwrapInt i0 + unwrapInt i1)
>                      where unwrapInt (I x) = x

> eval e (Var s) = lookup s e

> test = runEval $ eval Map.empty (Var "foo")
> test2 = runEval $ eval Map.empty (Add (Const (I 1)) (Const (I 2)))
> test3 = runEval $ eval Map.empty (Add (Const (I 1)) (Const (B True)))

and so on.
                             
                             
