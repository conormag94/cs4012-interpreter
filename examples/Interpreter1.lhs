module Interpreter1

This first version of the interpreter has no statements and only a
simple (non-monadic) evaluator. 

In the future versions we will move away from this design idea

> import qualified Data.Map as Map
> import Data.Maybe

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

> eval :: Env -> Expr -> Val
> eval _ (Const v) = v
> eval e (Add e0 e1) = let i0 = unwrapInt (eval e e0) 
>                          i1 = unwrapInt (eval e e1)
>                      in I (i0 + i1)
>                      where unwrapInt (I x) = x

> eval e (Var s) = fromJust $ Map.lookup s e

and so on.
                             
