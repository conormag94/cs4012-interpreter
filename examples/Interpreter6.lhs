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

{--- Adding simple statements to the language ---}

{-- A statement is a simple assignment, a condition, 
 -- a print, or a compound statement 
 --}


> data Statement = Assign String Expr
>                | If Expr Statement Statement
>                | Print Expr
>                | Seq Statement Statement

-- A running program exists in the State monad, transforming the Error
-- monad. So we will hand around a state value (containing the environment
-- which lists all the current variable bindings), and use the Exception
-- monad to manage any failure in expression evaluation

> type Run a = StateT Env (ExceptT String Identity) a 
> runRun p = runIdentity ( runExceptT ( runStateT p Map.empty) )


-- An operation to update the environment map

> set (s,i) = state $ (\table -> ((), Map.insert s i table))


-- Interpretation functions for each kind of statement

> exec (Assign s v) = do st <- get  
>                        Right val <- return $ runEval st (eval v)  
>                        set (s,val)

> exec (Seq s0 s1) = do exec s0 >> exec s1

> exec (Print e) = do st <- get
>                     Right val <- return $ runEval st (eval e) 
>                     return $ printout val
>                     return () 

> printout x = unsafePerformIO $ do hPutStrLn stdout $ show x


-- A very simple program to test the basic implementation
                                    
> prog1 = Assign "foo" (Const (I 1))  `Seq`
>         Print (Var "foo")

> testp :: Either String ( (), Env)
> testp =  runRun $ exec prog1
