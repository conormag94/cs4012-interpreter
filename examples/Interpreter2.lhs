> module Interpreter2 where
> import qualified Data.Map as Map
> import Data.Maybe

> import Control.Monad.Identity

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

{-- Monadic style, otherwise same as version 0 --}

> type Eval a = Identity a 
> runEval = runIdentity

> eval :: Env -> Expr -> Eval Val
> eval _ (Const v) = return v
> eval e (Add e0 e1) = do i0 <- eval e e0
>                         i1 <- eval e e1
>                         return $ I (unwrapInt i0 + unwrapInt i1)
>                      where unwrapInt (I x) = x

> eval e (Var s) = return $ fromJust $ Map.lookup s e

and so on.
