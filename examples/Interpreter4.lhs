> import Prelude hiding (lookup)
> import qualified Data.Map as Map
> import Data.Maybe

> import Control.Monad.Identity
> import Control.Monad.Except
> import Control.Monad.Reader

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

> lookup k t = case Map.lookup k t of
>                Just x -> return x
>                Nothing -> fail "lookup: key failed"

{-- Monadic style, adding error handling and Reader monad instance --}

> type Eval a = ReaderT Env (ExceptT String Identity) a 
> runEval env ex = runIdentity ( runExceptT ( runReaderT ex env) )

> eval :: Expr -> Eval Val
> eval (Const v) = return v
> eval (Add e0 e1) = do e0' <- eval e0
>                       e1' <- eval e1
>                       case (e0', e1') of                             
>                         (I i0, I i1) -> return $ I (i0 + i1)
>                         _            -> fail "type error in add"
> eval (Var s) = do env <- ask
>                   lookup s env

> test = runEval Map.empty (eval $ Var "foo")
> test2 = runEval Map.empty (eval $ Add (Const (I 1)) (Const (I 2)))
> test3 = runEval Map.empty (eval $ Add (Const (I 1)) (Const (B True)))

