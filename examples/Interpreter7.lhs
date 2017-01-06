> {-# Language FlexibleContexts, NoMonomorphismRestriction#-} 
> import Prelude hiding (lookup)
> import qualified Data.Map as Map
> import Data.Maybe

> import Control.Monad.Identity
> import Control.Monad.Except
> import Control.Monad.Reader
> import Control.Monad.State

> import System.IO.Unsafe
> import System.IO

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


> data Statement = Assign String Expr
>                | If Expr Statement Statement
>                | Print Expr
>                | Seq Statement Statement


-- A running program no longer transforms the Identity monad
-- but the IO monad. This means that every Run action is also
-- potentially an IO action, which means we no longer need the
-- unreliable and unsafe approach to printing of the previous version

> type Run a = StateT Env (ExceptT String IO) a 
> runRun p =  runExceptT ( runStateT p Map.empty) 

> set (s,i) = state $ (\table -> ((), Map.insert s i table))

> exec (Assign s v) = do st <- get  
>                        Right val <- return $ runEval st (eval v)  
>                        set (s,val)

> exec (Seq s0 s1) = do exec s0 >> exec s1

> exec (Print e) = do st <- get
>                     Right val <- return $ runEval st (eval e) 
>                     printout val
>                     return () 

-- Here we use lift to access the inner IO monad that we wrapped

> printout = lift . lift . System.IO.print

> prog1 = Assign "foo" (Const (I 1))  `Seq`
>         Print (Var "foo")

> testp :: IO (Either String ((), Env))
> testp =  runRun $ exec prog1
